------------------------------------------------------------------------------


------------------------------------------------------------------------------
module AI.NEAT.Config
       (
         NEATConfig (..),
         defaultNEATConfig
       ) where


------------------------------------------------------------------------------
data NEATConfig =
  NEATConfig { addNeuronRate             :: Double

             , activationMutationRate    :: Double
             , maxActivationPerturbation :: Double
             }


------------------------------------------------------------------------------
defaultNEATConfig :: NEATConfig
defaultNEATConfig =
  NEATConfig { addNeuronRate             = 0.08

             , activationMutationRate    = 0.1
             , maxActivationPerturbation = 0.1
             }
