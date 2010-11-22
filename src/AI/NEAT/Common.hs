------------------------------------------------------------------------------


------------------------------------------------------------------------------
module AI.NEAT.Common
       -- TODO: export list
       where


------------------------------------------------------------------------------
import Data.Graph.Inductive ( Node )


------------------------------------------------------------------------------
type NeuronId = Node


------------------------------------------------------------------------------
type LinkId = Node


------------------------------------------------------------------------------
data NeuronType
  = Input
  | Output
  | Bias
  | Hidden
  deriving (Show, Eq)


------------------------------------------------------------------------------
-- | Determines whether neuron is sensor by its type.
isSensor :: NeuronType -> Bool
isSensor Bias  = True
isSensor Input = True
isSensor _     = False
