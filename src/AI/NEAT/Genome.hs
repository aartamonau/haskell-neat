------------------------------------------------------------------------------


------------------------------------------------------------------------------
module AI.NEAT.Genome
       -- TODO: export list
       where


------------------------------------------------------------------------------
import Control.Monad ( replicateM )
import Data.Graph.Inductive ( Gr, mkGraph )


import AI.NEAT.Monad ( NEAT, random )

import AI.NEAT.Common ( NeuronId, NeuronType (..) )

import AI.NEAT.Genome.Neuron ( NeuronGene (..), neuronGene )
import qualified AI.NEAT.Genome.Neuron as Neuron

import AI.NEAT.Genome.Link ( LinkGene (..), linkGene )
import qualified AI.NEAT.Genome.Link as Link


------------------------------------------------------------------------------
data Genome =
  Genome { graph :: Gr NeuronGene LinkGene }


------------------------------------------------------------------------------
genome :: Int -> Int -> NEAT Genome
genome inputs outputs = do
  is    <- replicateM inputs (neuronGene Input)
  bias  <- neuronGene Bias
  os    <- replicateM outputs (neuronGene Output)

  links <- sequence $ [ linkGene (Neuron.id x) (Neuron.id y) =<< random
                            | x <- bias : is
                            , y <- os ]

  return $ Genome $ mkGraph [ (Neuron.id n, n) | n <- is ++ [bias] ++ os ]
                            [ (Link.from l, Link.to l, l) | l <- links ]
