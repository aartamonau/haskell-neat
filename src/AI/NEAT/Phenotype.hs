------------------------------------------------------------------------------
{-# LANGUAGE ViewPatterns #-}


------------------------------------------------------------------------------


------------------------------------------------------------------------------
module AI.NEAT.Phenotype
       (
         NeuralNet,

         update,
         getOutputs,
         fromGenome
       ) where


------------------------------------------------------------------------------
import Data.Graph.Inductive              ( context, lab', gmap )
import Data.Graph.Inductive.PatriciaTree ( Gr )
import Data.List ( foldl' )

import AI.NEAT.Common           ( NeuronType ( Input ) )
import AI.NEAT.Genome           ( Genome )
import qualified AI.NEAT.Genome as Genome
import AI.NEAT.Genome.Neuron    ( NeuronGene )
import AI.NEAT.Genome.Link      ( LinkGene )
import AI.NEAT.Phenotype.Neuron ( Neuron, neuron )
import qualified AI.NEAT.Phenotype.Neuron as Neuron
import AI.NEAT.Phenotype.Link   ( Link, link )
import qualified AI.NEAT.Phenotype.Link as Link
import AI.NEAT.Utils.Graph      ( bfsGNMap )


------------------------------------------------------------------------------
data NeuralNet =
  NeuralNet { inputs  :: !Int
            , outputs :: !Int
            , graph   :: !(Gr Neuron Link)
            }


------------------------------------------------------------------------------
-- | Lifts graph transformation into a genome.
liftGraphTransform :: (Gr Neuron Link -> Gr Neuron Link)
                   -> NeuralNet
                   -> NeuralNet
liftGraphTransform f nn = nn { graph = f (graph nn) }


------------------------------------------------------------------------------
-- | Recomputes outputs of all the neurons according to new input.
update :: NeuralNet -> [Double] -> NeuralNet
update nn vs = liftGraphTransform (bfsGNMap updateNeuron is) nn
  where updateNeuron gr (adj_a, node, neuron, adj_b) =
          Neuron.update neuron input

          where weights = map (Link.weight . fst) adj_a
                outputs = map (Neuron.output . get . snd) adj_a
                sum'    = foldl' (+) 0

                input | Neuron.tpy neuron == Input = vs !! node
                      | otherwise =
                           sum' $ zipWith (*) weights outputs

                get     = lab' . context gr

        is = [0 .. inputs nn]  -- inputs and bias


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
