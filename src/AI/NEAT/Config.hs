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

             , weightMutationRate        :: Double
             , maxWeightPerturbation     :: Double
             , newWeightChance           :: Double

             , activationMutationRate    :: Double
             , maxActivationPerturbation :: Double

             , loopedLinkRate            :: Double
             , loopedLinkTries           :: Int

             , linkTries                 :: Int

             , compatWeightExcess        :: Double
             , compatWeightDisjoint      :: Double
             , compatWeightMatched       :: Double
             }


------------------------------------------------------------------------------
defaultNEATConfig :: NEATConfig
defaultNEATConfig =
  NEATConfig { addNeuronRate             = 0.08

             , weightMutationRate        = 0.2
             , maxWeightPerturbation     = 0.5
             , newWeightChance           = 0.1

             , activationMutationRate    = 0.1
             , maxActivationPerturbation = 0.1

             , loopedLinkRate            = 0.1
             , loopedLinkTries           = 5

             , linkTries                 = 5

             , compatWeightExcess        = 1
             , compatWeightDisjoint      = 1
             , compatWeightMatched       = 0.4
             }
