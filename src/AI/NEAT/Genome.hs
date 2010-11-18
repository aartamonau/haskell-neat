------------------------------------------------------------------------------
{-# LANGUAGE PatternGuards #-}


------------------------------------------------------------------------------
module AI.NEAT.Genome
       -- TODO: export list
       where


------------------------------------------------------------------------------
import Control.Arrow ( (>>>) )
import Control.Monad ( replicateM )

import Data.Graph.Inductive ( mkGraph, context, lab', inn', out',
                              insNode, insEdges )
import Data.Graph.Inductive.PatriciaTree ( Gr )


import AI.NEAT.Monad ( NEAT, random, randomIntR, diceRoll, neuronsCount )
import AI.NEAT.Config ( NEATConfig ( addNeuronRate ) )

import AI.NEAT.Common ( NeuronId, NeuronType (..) )

import AI.NEAT.Genome.Neuron ( NeuronGene (..), neuronGene )
import qualified AI.NEAT.Genome.Neuron as Neuron

import AI.NEAT.Genome.Link ( LinkGene, linkGene )
import qualified AI.NEAT.Genome.Link as Link

import AI.NEAT.Utils.Graph ( modifyEdges )


------------------------------------------------------------------------------
data Genome =
  Genome { graph :: Gr NeuronGene LinkGene }


------------------------------------------------------------------------------
genome :: Int -> Int -> NEAT Genome
genome inputs outputs = do
  is    <- replicateM inputs (neuronGene Input)
  bias  <- neuronGene Bias
  os    <- replicateM outputs (neuronGene Output)

  links <- sequence $ [ linkGene x y =<< random | x <- bias : is, y <- os ]

  return $ Genome $ mkGraph [ Neuron.toLNode n | n <- is ++ [bias] ++ os ]
                            [ Link.toLEdge l   | l <- links ]


------------------------------------------------------------------------------
    -- TODO: size threshold
    -- TODO: bias to old links
    -- TODO: innovations
addNeuron :: Genome -> NEAT Genome
addNeuron genome = diceRoll addNeuronRate (return genome) addNeuronLoop
  where addNeuronLoop = do
          link <- randomLink genome
          if suitableLink link
            then doAddNeuron link
            else addNeuronLoop

        doAddNeuron (src, link, dst) = do
          neuron <- neuronGene Hidden

          link_a <- linkGene src neuron 1
          link_b <- linkGene neuron dst (Link.weight link)


          return . Genome $
            (insNode     (Neuron.toLNode neuron)                    >>>
             insEdges    [Link.toLEdge link_a, Link.toLEdge link_b] >>>
             modifyEdges (Neuron.id src, Neuron.id dst)
                         (\l -> l { Link.isEnabled = False }))
            (graph genome)

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
