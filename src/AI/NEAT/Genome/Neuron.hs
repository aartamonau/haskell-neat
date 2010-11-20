------------------------------------------------------------------------------


------------------------------------------------------------------------------
module AI.NEAT.Genome.Neuron
       ( NeuronType (..), NeuronGene (..),
         neuronGene,
         toLNode
       ) where


------------------------------------------------------------------------------
import Data.Graph.Inductive ( LNode )


------------------------------------------------------------------------------
import AI.NEAT.Common ( NeuronId, NeuronType (..) )
import AI.NEAT.Monad ( NEAT, getNeuronId )


------------------------------------------------------------------------------
-- | Neuron gene.
data NeuronGene =
  NeuronGene { id                 :: !NeuronId
             , tpy                :: !NeuronType
             , activationResponse :: !Double
             }


instance Show NeuronGene where
  show = show . AI.NEAT.Genome.Neuron.id


------------------------------------------------------------------------------
-- | Creates neuron gene of specified type.
neuronGene :: NeuronType -> NEAT NeuronGene
neuronGene tpy = do
  neuronId <- getNeuronId
  return $ NeuronGene neuronId tpy 1


------------------------------------------------------------------------------
toLNode :: NeuronGene -> LNode NeuronGene
toLNode neuron = (AI.NEAT.Genome.Neuron.id neuron, neuron)
