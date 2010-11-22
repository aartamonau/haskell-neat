------------------------------------------------------------------------------
{-# LANGUAGE PatternGuards #-}


------------------------------------------------------------------------------
module AI.NEAT.Genome
       -- TODO: export list
       where


------------------------------------------------------------------------------
import Control.Applicative ( (<$>), (<*>) )
import Control.Arrow ( (>>>) )
import Control.Exception ( assert )
import Control.Monad ( replicateM, join )
import Control.Monad.Reader ( asks )

import Data.Graph.Inductive ( LNode, Context,
                              mkGraph, context, lab', inn', out', pre',
                              insNode, insEdge, insEdges,
                              match, noNodes, labNodes )
import Data.Graph.Inductive.PatriciaTree ( Gr )

import Data.Maybe ( isNothing, isJust, fromJust )


import AI.NEAT.Monad ( NEAT,
                       random, randomR, randomIntR, diceRoll, neuronsCount,
                       findNeuronInnovation, findLinkInnovation )
import AI.NEAT.Config ( NEATConfig ( addNeuronRate,
                                     activationMutationRate,
                                     maxActivationPerturbation,
                                     weightMutationRate,
                                     maxWeightPerturbation,
                                     newWeightChance,
                                     loopedLinkTries,
                                     linkTries
                                   ) )

import AI.NEAT.Common ( NeuronId, NeuronType (..), isSensor )

import AI.NEAT.Genome.Neuron ( NeuronGene (..),
                               neuronGene, neuronGene_, neuronGeneHidden )
import qualified AI.NEAT.Genome.Neuron as Neuron

import AI.NEAT.Genome.Link ( LinkGene, linkGene, linkGene_ )
import qualified AI.NEAT.Genome.Link as Link

import AI.NEAT.Innovations.Neuron ( NeuronInnovation )
import qualified AI.NEAT.Innovations.Neuron as NInnovation

import AI.NEAT.Utils.Graph ( modifyEdges, nmapM, emapM )
import AI.NEAT.Utils.Monad ( matching, matchingTries )


------------------------------------------------------------------------------
data Genome =
  Genome { graph :: Gr NeuronGene LinkGene }


------------------------------------------------------------------------------
genome :: Int -> Int -> NEAT Genome
genome inputs outputs = do
  is    <- replicateM inputs (neuronGene Input)
  bias  <- neuronGene Bias
  os    <- replicateM outputs (neuronGene Output)

  links <- sequence [ linkGene x y =<< random | x <- bias : is, y <- os ]

  return $ Genome $ mkGraph [ Neuron.toLNode n | n <- is ++ [bias] ++ os ]
                            [ Link.toLEdge l   | l <- links ]


------------------------------------------------------------------------------
    -- TODO: size threshold
    -- TODO: bias to old links
addNeuron :: Genome -> NEAT Genome
addNeuron genome = diceRoll addNeuronRate (return genome) addNeuronLoop
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

                   return (link_a, link_a, neuron)

          return . Genome $
            (insNode     (Neuron.toLNode neuron)                    >>>
             insEdges    [Link.toLEdge link_a, Link.toLEdge link_b] >>>
             modifyEdges (Neuron.id src, Neuron.id dst)
                         (\l -> l { Link.isEnabled = False }))
            (graph genome)

          where lw = Link.weight link

        notInGenome inno =
          isNothing . fst $ match (NInnovation.id inno) (graph genome)

        suitableLink (src, link, _)
            | not (Link.isEnabled link) = False

            -- TODO: originally isRecurrent was used here instead; whether it
            -- really bad to split recurrent link?
            | Link.isLooped link        = False
            | Bias <- Neuron.tpy src    = False
            | otherwise                 = True


------------------------------------------------------------------------------
-- | Tries to add loop link in some node.
addLoopedLink :: Genome -> NEAT Genome
addLoopedLink g = do
  node <- matchingTries (randomGraphNode g) isSuitable =<< asks loopedLinkTries

  case node of
    Nothing -> return g
    Just ((_, neuron), _) -> do
      -- TODO: check whether innovation already exists
      link <- linkGene neuron neuron =<< random

      return $ Genome (insEdge (Link.toLEdge link) (graph g))

  where looped ctx = any (Link.isLooped . label) (out' ctx)
          where label (_, _, l) = l

        isSuitable ((_, ng), ctx) = not (isSensor $ Neuron.tpy ng) &&
                                    not (looped ctx)


------------------------------------------------------------------------------
-- | Tries to add link to genome.
addLink :: Genome -> NEAT Genome
addLink g = do
  -- TODO: ugly?
  edge <- join <$> (matchingTries findCandidate isJust =<< asks linkTries)

  case edge of
    Nothing -> return g
    Just (src, dst) -> do
      -- TODO: check whether innovation already exists
      link <- linkGene src dst =<< random

      return $ Genome (insEdge (Link.toLEdge link) (graph g))

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
randomGraphNode (Genome g) = do
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
mutateActivationResponses g = Genome <$> nmapM doMutate (graph g)
  where doMutate :: NeuronGene -> NEAT NeuronGene
        doMutate n =
          diceRoll activationMutationRate (return n) $ do
            ar <- mutateAR (Neuron.activationResponse n)
            return n { Neuron.activationResponse = ar }

        -- TODO: thread perturbation from outside?
        mutateAR :: Double -> NEAT Double
        mutateAR ar = do
          perturbation <- asks maxActivationPerturbation
          r            <- randomR (-perturbation, perturbation)

          return (ar + r)


------------------------------------------------------------------------------
-- | Mutates weights of the links.
mutateWeights :: Genome         -- ^ Genome to perform mutations on.
              -> NEAT Genome
mutateWeights g = Genome <$> emapM doMutate (graph g)
  where doMutate :: LinkGene -> NEAT LinkGene
        doMutate l =
          diceRoll weightMutationRate (return l) $
                   diceRoll newWeightChance (mutateWeight l) (newWeight l)

        newWeight :: LinkGene -> NEAT LinkGene
        newWeight l = random >>= \w -> return $ l { Link.weight = w }

        mutateWeight :: LinkGene -> NEAT LinkGene
        mutateWeight l = do
          perturbation <- asks maxWeightPerturbation
          r            <- randomR (-perturbation, perturbation)

          let weight = Link.weight l

          return $ l { Link.weight = r + weight }


------------------------------------------------------------------------------
-- TODO: mutateReenable?
-- TODO: mutateToggleEnabled?
