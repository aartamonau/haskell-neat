------------------------------------------------------------------------------


------------------------------------------------------------------------------
module AI.NEAT.Genome.Graphviz
       -- TODO: export list
       where


------------------------------------------------------------------------------
import Data.Graph.Inductive ( Node )
import Data.GraphViz ( Attributes, Attribute (..), Shape (..),
                       GraphvizParams (..), DotGraph,
                       GraphvizCanvas ( Xlib ),
                       graphToDot, defaultParams, nonClusteredParams,
                       runGraphvizCanvas' )


------------------------------------------------------------------------------
import AI.NEAT.Common        ( NeuronType (..) )

import AI.NEAT.Genome        ( Genome (..) )
import AI.NEAT.Genome.Neuron ( NeuronGene (..) )
import AI.NEAT.Genome.Link   ( LinkGene (..) )


------------------------------------------------------------------------------
inputAttrs :: Attributes
inputAttrs = []


------------------------------------------------------------------------------
outputAttrs :: Attributes
outputAttrs = [Shape DoubleCircle]


------------------------------------------------------------------------------
biasAttrs :: Attributes
biasAttrs = []


------------------------------------------------------------------------------
hiddenAttrs :: Attributes
hiddenAttrs = []


------------------------------------------------------------------------------
neuronGeneAttrs :: NeuronGene -> Attributes
neuronGeneAttrs (NeuronGene _ Input  _ _) = inputAttrs
neuronGeneAttrs (NeuronGene _ Output _ _) = outputAttrs
neuronGeneAttrs (NeuronGene _ Bias   _ _) = biasAttrs
neuronGeneAttrs (NeuronGene _ Hidden _ _) = hiddenAttrs


------------------------------------------------------------------------------
graphvizParams :: GraphvizParams NeuronGene LinkGene () NeuronGene
graphvizParams = nonClusteredParams { fmtNode = neuronGeneAttrs . nodeToGene }
  where nodeToGene = snd


------------------------------------------------------------------------------
toDot :: Genome -> DotGraph Node
toDot = graphToDot graphvizParams . graph


------------------------------------------------------------------------------
preview :: Genome -> IO ()
preview g = runGraphvizCanvas' (toDot g) Xlib >> return ()
