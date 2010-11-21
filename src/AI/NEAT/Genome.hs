------------------------------------------------------------------------------
{-# LANGUAGE PatternGuards #-}


------------------------------------------------------------------------------
module AI.NEAT.Genome
       -- TODO: export list
       where


------------------------------------------------------------------------------
import Control.Applicative ( (<$>) )
import Control.Arrow ( (>>>) )
import Control.Exception ( assert )
import Control.Monad ( replicateM )
import Control.Monad.Reader ( asks )

import Data.Graph.Inductive ( mkGraph, context, lab', inn', out',
                              insNode, insEdges, match )
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
                                     newWeightChance
                                   ) )

import AI.NEAT.Common ( NeuronId, NeuronType (..) )

import AI.NEAT.Genome.Neuron ( NeuronGene (..),
                               neuronGene, neuronGene_, neuronGeneHidden )
import qualified AI.NEAT.Genome.Neuron as Neuron

import AI.NEAT.Genome.Link ( LinkGene, linkGene, linkGene_ )
import qualified AI.NEAT.Genome.Link as Link

import AI.NEAT.Innovations.Neuron ( NeuronInnovation )
import qualified AI.NEAT.Innovations.Neuron as NInnovation

import AI.NEAT.Utils.Graph ( modifyEdges, nmapM, emapM )
import AI.NEAT.Utils.Monad ( matching )


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

        suitableLink (src, link, _) | not (Link.isEnabled link) = False
                                    | Link.isRecurrent link     = False
                                    | Bias <- Neuron.tpy src    = False
                                    | otherwise                 = True


------------------------------------------------------------------------------
randomNeuron :: Genome -> NEAT NeuronGene
randomNeuron g = do
  -- TODO: this will work only when the genome has all the neurons introduced
  -- during evolution of all the genomes inside the thread of evolution
  n <- randomIntR =<< neuronsCount
  return $ getNeuron g n


------------------------------------------------------------------------------
getNeuron :: Genome -> NeuronId -> NeuronGene
getNeuron g n = lab' $ context (graph g) n


------------------------------------------------------------------------------
randomLink :: Genome -> NEAT (NeuronGene, LinkGene, NeuronGene)
randomLink g = do
  n <- randomIntR =<< neuronsCount

  let ctx        = context (graph g) n
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
