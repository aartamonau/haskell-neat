------------------------------------------------------------------------------


------------------------------------------------------------------------------
module AI.NEAT.Phenotype.Neuron
       (
         Neuron ( tpy, activationResponse, output ),
         neuron
       ) where


------------------------------------------------------------------------------
import AI.NEAT.Common        ( NeuronType )
import AI.NEAT.Genome.Neuron ( NeuronGene )
import qualified AI.NEAT.Genome.Neuron as NeuronGene


------------------------------------------------------------------------------
data Neuron =
  Neuron { tpy                :: !NeuronType
         , activationResponse :: !Double

         , output             :: !Double
         }


------------------------------------------------------------------------------
-- | Constructs neuron from a gene.
neuron :: NeuronGene -> Neuron
neuron ngene =
  Neuron (NeuronGene.tpy ngene) (NeuronGene.activationResponse ngene) 0
