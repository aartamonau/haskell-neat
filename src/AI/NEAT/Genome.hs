------------------------------------------------------------------------------
{-# LANGUAGE ViewPatterns #-}


------------------------------------------------------------------------------
module AI.NEAT.Genome
       -- TODO: export list
       where


------------------------------------------------------------------------------
import Control.Applicative ( (<$>) )
import Control.Arrow ( (>>>) )
import Control.Exception ( assert )
import Control.Monad ( mapM, mapM_, join, ap )
import Control.Monad.Reader ( asks )

import Control.Monad.ST.Strict ( ST, runST )
import Data.STRef ( newSTRef, readSTRef, writeSTRef, modifySTRef )

import Data.Function ( on )
import Data.Graph.Inductive ( LNode, Context,
                              empty,
                              mkGraph, context, lab', inn', out', pre',
                              insNode, insNodes, insEdge, insEdges,
                              match, noNodes, labNodes, labEdges )
import Data.Graph.Inductive.PatriciaTree ( Gr )
import Data.List ( sortBy, nubBy, groupBy, foldl' )
import Data.Maybe ( isNothing, isJust, fromJust )


import AI.NEAT.Monad ( NEAT,
                       random, randomR, randomIntR, randomChoice, diceRoll,
                       findNeuronInnovation, findLinkInnovation )
import qualified AI.NEAT.Config as Config

import AI.NEAT.Common ( NeuronId, NeuronType (..), isSensor )

import AI.NEAT.Genome.Neuron ( NeuronGene,
                               neuronGene, neuronGene_, neuronGeneHidden )
import qualified AI.NEAT.Genome.Neuron as Neuron

import AI.NEAT.Genome.Link ( LinkGene, linkGene, linkGene_ )
import qualified AI.NEAT.Genome.Link as Link

import AI.NEAT.Innovations ( InnovationId )
import qualified AI.NEAT.Innovations.Neuron as NInnovation
import qualified AI.NEAT.Innovations.Link as LInnovation

import AI.NEAT.Utils.Graph ( modifyEdges, nmapM, emapM )
import AI.NEAT.Utils.Monad ( matching, matchingTries )


------------------------------------------------------------------------------
type GenomeId = Int


------------------------------------------------------------------------------
data Genome =
  Genome { id      :: !GenomeId
         , graph   :: !(Gr NeuronGene LinkGene) }


------------------------------------------------------------------------------
genome :: GenomeId -> NEAT Genome
genome gid = do
  inputs  <- asks Config.inputsNumber
  outputs <- asks Config.outputsNumber

  is    <- mapM (neuronGene Input) [0 .. inputs - 1]
  bias  <- neuronGene Bias inputs
  os    <- mapM (neuronGene Output) [inputs + 1 .. inputs + outputs]

  links <- sequence [ linkGene x y =<< random | x <- bias : is, y <- os ]

  let graph = mkGraph [ Neuron.toLNode n | n <- is ++ [bias] ++ os ]
                      [ Link.toLEdge l   | l <- links ]

  return $ Genome gid graph


------------------------------------------------------------------------------
-- | Lifts graph transformation into a genome.
liftGraphTransform :: (Gr NeuronGene LinkGene -> Gr NeuronGene LinkGene)
                   -> Genome
                   -> Genome
liftGraphTransform f genome = genome { graph = f (graph genome) }


------------------------------------------------------------------------------
-- | Lifts monadic graph transform into a genome.
liftGraphTransformM :: Monad m
                    => (Gr NeuronGene LinkGene -> m (Gr NeuronGene LinkGene))
                    -> Genome
                    -> m (Genome)
liftGraphTransformM f genome = do
  g <- f (graph genome)
  return $ genome { graph = g }


------------------------------------------------------------------------------
-- | Inserts neuron in the graph of genome.
insertNeuron :: NeuronGene -> Genome -> Genome
insertNeuron n = insertNeurons [n]


------------------------------------------------------------------------------
-- | Inserts series of neurons into the genome's graph.
insertNeurons :: [NeuronGene] -> Genome -> Genome
insertNeurons = liftGraphTransform . insNodes . map Neuron.toLNode


------------------------------------------------------------------------------
-- | Inserts a link into a genome's graph.
insertLink :: LinkGene -> Genome -> Genome
insertLink l = insertLinks [l]


------------------------------------------------------------------------------
-- | Inserts a series of links into a genome's graph.
insertLinks :: [LinkGene] -> Genome -> Genome
insertLinks = liftGraphTransform . insEdges . map Link.toLEdge


------------------------------------------------------------------------------
-- | 'modifyEdges' lifted into a genome. Singular since we expect only one
-- link between every two neurons.
modifyLink :: (NeuronGene, NeuronGene)
            -> (LinkGene -> LinkGene)
            -> Genome
            -> Genome
modifyLink (src, dst) = liftGraphTransform . modifyEdges edge
  where edge = (Neuron.id src, Neuron.id dst)


------------------------------------------------------------------------------
    -- TODO: size threshold
    -- TODO: bias to old links
addNeuron :: Genome -> NEAT Genome
addNeuron genome = diceRoll Config.addNeuronRate (return genome) addNeuronLoop
  where addNeuronLoop = do
          link <- matching (randomLink genome) suitableLink
          doAddNeuron link

        doAddNeuron (src, link, dst) = do
          -- TODO: eliminate those Neuron.id
          innovation <- findNeuronInnovation (Neuron.id src, Neuron.id dst)
                                              notInGenome

          (link_a, link_b, neuron)
            <- case innovation of
                 Nothing -> do
                   neuron <- neuronGeneHidden (Neuron.id src, Neuron.id dst)
                   link_a <- linkGene src neuron 1
                   link_b <- linkGene neuron dst lw

                   return (link_a, link_b, neuron)
                 Just inno -> do
                   let neuron = neuronGene_ inno

                   link_a_inno <- findLinkInnovation (Neuron.id src,
                                                      Neuron.id neuron)
                   link_b_inno <- findLinkInnovation (Neuron.id neuron,
                                                      Neuron.id dst)

                   let link_a = assert (isJust link_a_inno)
                                       (linkGene_ (fromJust link_a_inno) 1)
                   let link_b = assert (isJust link_b_inno)
                                       (linkGene_ (fromJust link_b_inno) lw)

                   return (link_a, link_b, neuron)

          return $ insertNeuron neuron               >>>
                   insertLinks [link_a, link_b]      >>>
                   modifyLink (src, dst) disableLink $ genome

          where lw = Link.weight link

        notInGenome inno =
          isNothing . fst $ match (NInnovation.id inno) (graph genome)

        suitableLink (src, link, _)
            | not (Link.isEnabled link) = False

            -- TODO: originally isRecurrent was used here instead; whether it
            -- really bad to split recurrent link?
            | Link.isLooped link        = False
            | Bias == Neuron.tpy src    = False
            | otherwise                 = True

        disableLink l = l { Link.isEnabled = False }


------------------------------------------------------------------------------
-- | Tries to add loop link in some node.
addLoopedLink :: Genome -> NEAT Genome
addLoopedLink g = do
  node <- matchingTries (randomGraphNode g) isSuitable
                                        =<< asks Config.loopedLinkTries

  case node of
    Nothing -> return g
    Just ((_, neuron), _) -> do
      -- TODO: check whether innovation already exists
      link <- linkGene neuron neuron =<< random

      return $ insertLink link g

  where looped ctx = any (Link.isLooped . label) (out' ctx)
          where label (_, _, l) = l

        isSuitable ((_, ng), ctx) = not (isSensor $ Neuron.tpy ng) &&
                                    not (looped ctx)


------------------------------------------------------------------------------
-- | Tries to add link to genome.
addLink :: Genome -> NEAT Genome
addLink g = do
  -- TODO: ugly?
  edge <- join <$> (matchingTries findCandidate isJust
                                            =<< asks Config.linkTries)

  case edge of
    Nothing -> return g
    Just (src, dst) -> do
      -- TODO: check whether innovation already exists
      link <- linkGene src dst =<< random

      return $ insertLink link g

  where findCandidate :: NEAT (Maybe (NeuronGene, NeuronGene))
        findCandidate = do
          src             <- randomNeuron g
          ((_, dst), ctx) <- randomGraphNode g

          let duplicate = any (== (Neuron.id src)) (pre' ctx)

          if isSensor (Neuron.tpy dst) ||
             duplicate                 ||
             (Neuron.id src == Neuron.id dst)

            then return Nothing
            else return $ Just (src, dst)


------------------------------------------------------------------------------
-- | Helper function that returns random node from genome's graph.
randomGraphNode :: Genome
                -> NEAT (LNode NeuronGene, Context NeuronGene LinkGene)
-- TODO: this is temporary O(n) version; this must work in O(1) time.
randomGraphNode (graph -> g) = do
  n <- randomIntR (noNodes g)
  let lnode@(node, _) = labNodes g !! n
  let (ctx, _)        = match node g

  return (lnode, fromJust ctx)


------------------------------------------------------------------------------
-- | Returns random neuron gene.
randomNeuron :: Genome -> NEAT NeuronGene
randomNeuron g = (snd . fst) <$> randomGraphNode g


------------------------------------------------------------------------------
getNeuron :: Genome -> NeuronId -> NeuronGene
getNeuron g n = lab' $ context (graph g) n


------------------------------------------------------------------------------
randomLink :: Genome -> NEAT (NeuronGene, LinkGene, NeuronGene)
randomLink g = do
  (_, ctx) <- randomGraphNode g

  let links      = inn' ctx ++ out' ctx
  let linksCount = length links

  -- by construction there are no nodes which does not have neither
  -- inward-directed nor outward-directed links

  -- TODO: maybe something better than this
  m <- randomIntR linksCount

  let link = label (links !! m)

  return (getNeuron g (Link.from link),
          link,
          getNeuron g (Link.to link))

  where label (_, _, l) = l


------------------------------------------------------------------------------
-- | Mutates activation responses of neurons.
mutateActivationResponses :: Genome -- ^ Genome to perform mutations on.
                          -> NEAT Genome
mutateActivationResponses = liftGraphTransformM $ nmapM doMutate
  where doMutate :: NeuronGene -> NEAT NeuronGene
        doMutate n
          | Neuron.tpy n == Input ||
            Neuron.tpy n == Bias = return n
          | otherwise = diceRoll Config.activationMutationRate (return n) $ do
                          ar <- mutateAR (Neuron.activationResponse n)
                          return n { Neuron.activationResponse = ar }

        -- TODO: thread perturbation from outside?
        mutateAR :: Double -> NEAT Double
        mutateAR ar = do
          perturbation <- asks Config.maxActivationPerturbation
          r            <- randomR (-perturbation, perturbation)

          return (ar + r)


------------------------------------------------------------------------------
-- | Mutates weights of the links.
mutateWeights :: Genome         -- ^ Genome to perform mutations on.
              -> NEAT Genome
mutateWeights = liftGraphTransformM $ emapM doMutate
  where doMutate :: LinkGene -> NEAT LinkGene
        doMutate l =
          diceRoll Config.weightMutationRate (return l) $
                   diceRoll Config.newWeightChance
                            (mutateWeight l) (newWeight l)

        newWeight :: LinkGene -> NEAT LinkGene
        newWeight l = random >>= \w -> return $ l { Link.weight = w }

        mutateWeight :: LinkGene -> NEAT LinkGene
        mutateWeight l = do
          perturbation <- asks Config.maxWeightPerturbation
          r            <- randomR (-perturbation, perturbation)

          let weight = Link.weight l

          return $ l { Link.weight = r + weight }


------------------------------------------------------------------------------
-- TODO: mutateReenable?
-- TODO: mutateToggleEnabled?


------------------------------------------------------------------------------
-- TODO: common
type Fitness = Double


------------------------------------------------------------------------------
-- | Returns a list o link genes of genome accompanied by the innovation id in
-- which that link has been introduced. The list is sorted by innovation id.

-- TODO: innovation id can really be stored in link gene for performance
linkGenes :: Genome
          -> NEAT [(InnovationId, LinkGene)]
linkGenes (graph -> g) = do
  sortBy (compare `on` fst) <$> (mapM (attachInnovation . label) $ labEdges g)
  where getInnovation link = do
          innovation <- findLinkInnovation (Link.from link, Link.to link)
          return $ assert (isJust innovation)
                          (LInnovation.id $ fromJust innovation)

        attachInnovation link = do
          innovation <- getInnovation link
          return (innovation, link)

        label (_, _, l) = l


------------------------------------------------------------------------------
-- | Tag describing the origin of the object in a mergin procedure.
data MergeTag a = MLeft  a
                | MRight a
                | MBoth  a a


------------------------------------------------------------------------------
-- | Compares two genomes and returns a list of all the links available in
-- either of them. Each link is tagged by its origin.
origLinkGenes :: Genome
              -> Genome
              -> NEAT [(InnovationId, MergeTag LinkGene)]
origLinkGenes left right = do
  leftGenes  <- linkGenes left
  rightGenes <- linkGenes right

  return $ go leftGenes rightGenes

  where go [] rs = tags MRight rs
        go ls [] = tags MLeft  ls
        go ls@((lid, l) : ls') rs@((rid, r) : rs')
            | lid < rid = (lid, MLeft  l)   : go ls' rs
            | lid > rid = (rid, MRight r)   : go ls  rs'
            | otherwise = (rid, MBoth  r l) : go ls' rs'

        tag f (id, link) = (id, f link)
        tags f           = map (tag f)


------------------------------------------------------------------------------
crossover :: GenomeId
          -> (Genome, Fitness)
          -> (Genome, Fitness)
          -> NEAT Genome
crossover gid gx gy = do
  (offspringLinks, offspringNeurons) <-
      doCrossover =<< origLinkGenes winner loser

  return $ insertNeurons offspringNeurons >>>
           insertLinks   offspringLinks   $ emptyGenome

  where swap (x, fx) (y, fy) | fx >= fy  = (x, y)
                             | otherwise = (y, x)

        (winner, loser) = swap gx gy

        emptyGenome = Genome gid empty

        doCrossover os = do
            (links, neurons) <- fmap unzip $ sequence $ foldr (k . snd) [] os

            -- TODO: use Data.Set here
            return (links, nubBy ((==) `on` Neuron.id) $ concat neurons)
            where k (MLeft  x)  xs = return (x, neurons winner x) : xs
                  k (MRight _)  xs = xs
                  k (MBoth x y) xs = randomChoice (x, neurons winner x)
                                                  (y, neurons loser y) : xs

        neurons genome link = [getNeuron genome (Link.from link),
                               getNeuron genome (Link.to link)]


-- | Temporary function to test crossover.
crossover_ :: GenomeId -> Genome -> Genome -> NEAT Genome
crossover_ gid x y = crossover gid (x, 1) (y, 0)


------------------------------------------------------------------------------
-- | Returns compatibility distance for two genomes.
compatibility :: Genome -> Genome -> NEAT Double
compatibility a b = do
  links <- segment . map snd <$> origLinkGenes a b
  let stats = runST (getStatsST links)

  weightExcess   <- asks Config.compatWeightExcess
  weightDisjoint <- asks Config.compatWeightDisjoint
  weightMatched  <- asks Config.compatWeightMatched

  let distance = (weightExcess   * (excess stats)           / (longest stats)) +
                 (weightDisjoint * (disjoint stats)         / (longest stats)) +
                 (weightMatched  * (weightDifference stats) / (matched stats))

  return distance

  where segment :: [MergeTag LinkGene] -> [MergeTag [LinkGene]]
        segment = map join . groupBy p
          where p (MLeft  _)   (MLeft  _)   = True
                p (MRight _)   (MRight _)   = True
                p (MBoth  _ _) (MBoth  _ _) = True
                p _          _              = False

                join (MLeft  x : xs)  = MLeft  (x : map unwrap xs)
                join (MRight x : xs)  = MRight (x : map unwrap xs)
                join (MBoth x y : xs) = MBoth  (x : xs') (y : ys')
                  where (xs', ys') = unzip $ map unwrap2 xs

                unwrap (MLeft x)  = x
                unwrap (MRight x) = x
                unwrap _          = assert False (error "assertion")

                unwrap2 (MBoth x y) = (x, y)
                unwrap2 _           = assert False (error "assertion")

        getStatsST :: [MergeTag [LinkGene]] -> ST s (Int, Int, Int, Int, Double)
        getStatsST segments = do
          matched          <- newSTRef 0
          excess           <- newSTRef 0
          disjoint         <- newSTRef 0
          weightDifference <- newSTRef 0

          lengthLeft       <- newSTRef 0
          lengthRight      <- newSTRef 0

          let updateDisjoint n length = do
                excess_ <- readSTRef excess
                modifySTRef disjoint (+excess_)
                writeSTRef excess n
                modifySTRef length (+n)

          let updateMatched (map Link.weight -> ls1)
                            (map Link.weight -> ls2) = do
                modifySTRef matched (+n)
                modifySTRef lengthLeft  (+n)
                modifySTRef lengthRight (+n)

                let diff = foldl' (+) 0 $ zipWith ((abs.) . (-)) ls1 ls2
                modifySTRef weightDifference (+diff)

                where n = length ls1

          let update (MLeft  ls)      = updateDisjoint (length ls) lengthLeft
              update (MRight ls)      = updateDisjoint (length ls) lengthRight
              update (MBoth  ls1 ls2) = updateMatched ls1 ls2

          mapM_ update segments

          longest <- return max `ap` readSTRef lengthLeft
                                `ap` readSTRef lengthRight

          -- No Applicative instance for ST
          return (,,,,) `ap` readSTRef matched
                        `ap` readSTRef excess
                        `ap` readSTRef disjoint
                        `ap` (return longest)
                        `ap` readSTRef weightDifference

        matched          (m, _, _, _, _) = fromIntegral m
        excess           (_, e, _, _, _) = fromIntegral e
        disjoint         (_, _, d, _, _) = fromIntegral d
        longest          (_, _, _, l, _) = fromIntegral l
        weightDifference (_, _, _, _, w) = w
