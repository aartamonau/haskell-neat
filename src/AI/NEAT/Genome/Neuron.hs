------------------------------------------------------------------------------


------------------------------------------------------------------------------
module AI.NEAT.Genome.Neuron
       ( NeuronType (..), NeuronGene (..), neuronGene )
       -- TODO: export list
       where


------------------------------------------------------------------------------
import AI.NEAT.Common ( NeuronId, NeuronType (..) )
import AI.NEAT.Monad ( NEAT, getNeuronId )


------------------------------------------------------------------------------
-- | Neuron gene.
data NeuronGene =
  NeuronGene { id                 :: !NeuronId
             , tpy                :: !NeuronType
             , recurrent          :: !Bool
             , activationResponse :: !Double
             }


instance Show NeuronGene where
  show = show . AI.NEAT.Genome.Neuron.id


------------------------------------------------------------------------------
-- TODO: innovations
neuronGene :: NeuronType -> NEAT NeuronGene
neuronGene tpy = do
  neuronId <- getNeuronId
  return $ NeuronGene neuronId tpy False 1
