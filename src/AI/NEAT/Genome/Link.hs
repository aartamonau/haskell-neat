------------------------------------------------------------------------------


------------------------------------------------------------------------------
module AI.NEAT.Genome.Link
       -- TODO: export list
       where


------------------------------------------------------------------------------
import Data.Graph.Inductive ( LEdge )


------------------------------------------------------------------------------
import AI.NEAT.Common ( NeuronId )
import AI.NEAT.Monad ( NEAT )

import AI.NEAT.Genome.Neuron ( NeuronGene )
import qualified AI.NEAT.Genome.Neuron as Neuron


------------------------------------------------------------------------------
-- TODO: Innovations
-- TODO: Maybe it's sensible to embed neuron genes here (to avoid auxiliarry
--       queries to graph)
data LinkGene =
  LinkGene { from      :: !NeuronId
           , to        :: !NeuronId
           , weight    :: !Double
           , isEnabled :: !Bool
           }


instance Show LinkGene where
  show = show . weight


------------------------------------------------------------------------------
-- TODO: innovations
linkGene :: NeuronGene -> NeuronGene -> Double -> NEAT LinkGene
linkGene from to weight = return $ LinkGene (Neuron.id from)
                                            (Neuron.id to)
                                             weight
                                             True


------------------------------------------------------------------------------
isRecurrent :: LinkGene -> Bool
isRecurrent link = (from link) == (to link)


------------------------------------------------------------------------------
toLEdge :: LinkGene -> LEdge LinkGene
toLEdge link = (from link, to link, link)
