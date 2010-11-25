------------------------------------------------------------------------------


------------------------------------------------------------------------------
module AI.NEAT.Phenotype
       (
         NeuralNet
       ) where


------------------------------------------------------------------------------
import Data.Graph.Inductive              ( context, lab' )
import Data.Graph.Inductive.PatriciaTree ( Gr )

import AI.NEAT.Genome           ( Genome )
import AI.NEAT.Genome.Neuron    ( NeuronGene )
import AI.NEAT.Genome.Link      ( LinkGene )
import AI.NEAT.Phenotype.Neuron ( Neuron )
import qualified AI.NEAT.Phenotype.Neuron as Neuron
import AI.NEAT.Phenotype.Link   ( Link )


------------------------------------------------------------------------------
data NeuralNet =
  NeuralNet { inputs  :: !Int
            , outputs :: !Int
            , net     :: !(Gr Neuron Link)
            }


------------------------------------------------------------------------------
-- | Recomputes outputs of all the neurons according to new input.
update :: NeuralNet -> [Double] -> NeuralNet
update = undefined


------------------------------------------------------------------------------
-- | Returns current output of neural net.
getOutputs :: NeuralNet -> [Double]
getOutputs (NeuralNet inp outp gr) = map (Neuron.output . get) ixs
  where ixs = [inp + 1 .. inp + outp]
        get = lab' . context gr


------------------------------------------------------------------------------
-- | Builds neural net from genome.
fromGenome :: Genome -> NeuralNet
fromGenome = undefined
  where neuronFromGene :: NeuronGene -> Neuron
        neuronFromGene = undefined

        linkFromGene :: LinkGene -> Link
        linkFromGene = undefined
