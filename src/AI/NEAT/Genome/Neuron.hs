------------------------------------------------------------------------------


------------------------------------------------------------------------------
module AI.NEAT.Genome.Neuron
       ( NeuronType (..), NeuronGene (..),
         neuronGene,
         neuronGene_,
         neuronGeneHidden,
         toLNode
       ) where


------------------------------------------------------------------------------
import Data.Graph.Inductive ( LNode )


------------------------------------------------------------------------------
import AI.NEAT.Common ( NeuronId, NeuronType (..) )
import AI.NEAT.Monad ( NEAT, getNeuronId, createNeuronInnovation )

import AI.NEAT.Innovations.Neuron ( NeuronInnovation )
import qualified AI.NEAT.Innovations.Neuron as NInnovation


------------------------------------------------------------------------------
-- | Neuron gene.
data NeuronGene =
  NeuronGene { id                 :: !NeuronId
             , tpy                :: !NeuronType
             , activationResponse :: !Double
             }
  deriving Show


------------------------------------------------------------------------------
-- | Creates neuron gene of specified type.
neuronGene :: NeuronType -> NEAT NeuronGene
neuronGene tpy = do
  neuronId <- getNeuronId
  return $ NeuronGene neuronId tpy 1


------------------------------------------------------------------------------
-- | Helper function that creates neuron gene of hidden type and corresponding
-- innovation.
neuronGeneHidden :: (NeuronId, NeuronId) -> NEAT NeuronGene
neuronGeneHidden edge = do
  neuronId <- getNeuronId
  _        <- createNeuronInnovation edge neuronId

  return $ NeuronGene neuronId Hidden 1


------------------------------------------------------------------------------
-- | Creates neuron gene based on the information stored in neuron innovation.
neuronGene_ :: NeuronInnovation -> NeuronGene
neuronGene_ inno = NeuronGene (NInnovation.id inno) Hidden 1


------------------------------------------------------------------------------
toLNode :: NeuronGene -> LNode NeuronGene
toLNode neuron = (AI.NEAT.Genome.Neuron.id neuron, neuron)
