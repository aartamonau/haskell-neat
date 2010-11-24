------------------------------------------------------------------------------


------------------------------------------------------------------------------
module AI.NEAT.Phenotype.Neuron
       (
         Neuron ( tpy, activationResponse, output )
       ) where


------------------------------------------------------------------------------
import AI.NEAT.Common ( NeuronType )


------------------------------------------------------------------------------
data Neuron =
  Neuron { tpy                :: !NeuronType
         , activationResponse :: !Double

         , output             :: !Double
         }
