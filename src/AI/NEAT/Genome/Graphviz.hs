------------------------------------------------------------------------------


------------------------------------------------------------------------------
module AI.NEAT.Genome.Graphviz
       -- TODO: export list
       where


------------------------------------------------------------------------------
import Data.Graph.Inductive ( Node )
import Data.GraphViz ( GlobalAttributes (..), Attributes, Attribute (..),
                       Shape (..), GraphID ( Str ),
                       GraphvizParams (..), DotGraph,
                       GraphvizCanvas ( Xlib ), Color (..), X11Color (..),
                       StyleItem (..), StyleName (..),
                       LNodeCluster, NodeCluster (..), Label ( StrLabel ),
                       graphToDot, defaultParams,
                       runGraphvizCanvas' )

import Text.Printf ( printf )


------------------------------------------------------------------------------
import AI.NEAT.Common        ( NeuronType (..) )

import AI.NEAT.Genome        ( Genome (..) )
import AI.NEAT.Genome.Neuron ( NeuronGene (..) )
import AI.NEAT.Genome.Link   ( LinkGene (..) )


------------------------------------------------------------------------------
data Cluster = None
             | Inputs
             | Outputs
             deriving (Eq, Ord)


------------------------------------------------------------------------------
clusterize :: NeuronGene -> Cluster
clusterize (NeuronGene _ Input  _ _) = Inputs
clusterize (NeuronGene _ Bias   _ _) = Inputs
clusterize (NeuronGene _ Output _ _) = Outputs
clusterize (NeuronGene _ Hidden _ _) = None


------------------------------------------------------------------------------
clusterName :: Cluster -> String
clusterName Inputs  = "inputs"
clusterName Outputs = "outputs"
clusterName _       = error "invalid cluster supplied to clusterName function"


------------------------------------------------------------------------------
clusterAttrs :: Cluster -> [GlobalAttributes]
clusterAttrs cluster = [ GraphAttrs [ Label (StrLabel $ clusterName cluster) ] ]


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
linkGeneAttrs :: LinkGene -> Attributes
linkGeneAttrs link = [ Label (StrLabel (printf "%.3f" $ weight link))
                     , style
                     ]
  where style | enabled link = Style [SItem Solid []]
              | otherwise    = Style [SItem Dashed []]


------------------------------------------------------------------------------
graphvizParams :: GraphvizParams NeuronGene LinkGene Cluster NeuronGene
graphvizParams =
  defaultParams { globalAttributes = globalAttrs
                , clusterBy        = genomeClusterBy
                , clusterID        = Just . Str . clusterName
                , fmtCluster       = clusterAttrs
                , fmtNode          = neuronGeneAttrs . nodeToGene
                , fmtEdge          = linkGeneAttrs . linkToGene
                }
  where nodeToGene           = snd
        linkToGene (_, _, g) = g

        genomeClusterBy lnode =
          case clusterize (nodeToGene lnode) of
            None    -> N lnode
            Inputs  -> C Inputs (N lnode)
            Outputs -> C Outputs (N lnode)


------------------------------------------------------------------------------
toDot :: Genome -> DotGraph Node
toDot = graphToDot graphvizParams . graph


------------------------------------------------------------------------------
preview :: Genome -> IO ()
preview g = runGraphvizCanvas' (toDot g) Xlib >> return ()
