------------------------------------------------------------------------------


------------------------------------------------------------------------------
module AI.NEAT.Phenotype
       (
         NeuralNet
       ) where


------------------------------------------------------------------------------
import Data.Graph.Inductive              ( context, lab', gmap )
import Data.Graph.Inductive.PatriciaTree ( Gr )

import AI.NEAT.Genome           ( Genome )
import qualified AI.NEAT.Genome as Genome
import AI.NEAT.Genome.Neuron    ( NeuronGene )
import AI.NEAT.Genome.Link      ( LinkGene )
import AI.NEAT.Phenotype.Neuron ( Neuron, neuron )
import qualified AI.NEAT.Phenotype.Neuron as Neuron
import AI.NEAT.Phenotype.Link   ( Link, link )


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
fromGenome genome = NeuralNet (Genome.inputs genome)
                              (Genome.outputs genome)
                              (gmap transform $ Genome.graph genome)
  where transform (adj_a, node, ngene, adj_b) =
          (map transformAdj adj_a, node, neuron ngene, map transformAdj adj_b)

        transformAdj (lgene, node) = (link lgene, node)
