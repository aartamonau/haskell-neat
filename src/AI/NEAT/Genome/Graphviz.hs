------------------------------------------------------------------------------


------------------------------------------------------------------------------
module AI.NEAT.Genome.Graphviz
       -- TODO: export list
       where


------------------------------------------------------------------------------
import Data.Graph.Inductive ( Node )
import Data.GraphViz ( GlobalAttributes (..), Attributes, Attribute (..),
                       Shape (..),
                       GraphvizParams (..), DotGraph,
                       GraphvizCanvas ( Xlib ), Color (..), X11Color (..),
                       StyleItem (..), StyleName ( Filled ),
                       graphToDot, defaultParams, nonClusteredParams,
                       runGraphvizCanvas' )


------------------------------------------------------------------------------
import AI.NEAT.Common        ( NeuronType (..) )

import AI.NEAT.Genome        ( Genome (..) )
import AI.NEAT.Genome.Neuron ( NeuronGene (..) )
import AI.NEAT.Genome.Link   ( LinkGene (..) )


------------------------------------------------------------------------------
globalAttrs :: [GlobalAttributes]
globalAttrs = [ NodeAttrs [ Style [SItem Filled []]
                          , Shape Circle
                          , FillColor (X11Color White)
                          ]
              ]


------------------------------------------------------------------------------
inputAttrs :: Attributes
inputAttrs = [ FillColor (X11Color Gray), PenWidth 3 ]


------------------------------------------------------------------------------
biasAttrs :: Attributes
biasAttrs = [ FillColor (X11Color DarkSlateGray), PenWidth 3 ]


------------------------------------------------------------------------------
outputAttrs :: Attributes
outputAttrs = [ Shape DoubleCircle, FillColor (X11Color Gray) ]


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
graphvizParams =
  nonClusteredParams { globalAttributes = globalAttrs
                     , fmtNode          = neuronGeneAttrs . nodeToGene
                     }
  where nodeToGene = snd


------------------------------------------------------------------------------
toDot :: Genome -> DotGraph Node
toDot = graphToDot graphvizParams . graph


------------------------------------------------------------------------------
preview :: Genome -> IO ()
preview g = runGraphvizCanvas' (toDot g) Xlib >> return ()
