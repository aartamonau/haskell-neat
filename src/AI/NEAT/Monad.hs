------------------------------------------------------------------------------
{-# LANGUAGE GeneralizedNewtypeDeriving #-}


------------------------------------------------------------------------------
module AI.NEAT.Monad
       -- TODO: export list
       where


------------------------------------------------------------------------------
import Control.Monad.Trans              ( MonadTrans, lift )

import Control.Monad.Reader             ( MonadReader )
import Control.Monad.Trans.Reader       ( ReaderT, runReaderT )

import Control.Monad.State              ( MonadState, gets, modify )
import Control.Monad.Trans.State.Strict ( StateT, evalStateT )

import Control.Monad.Mersenne.Random    ( Rand, evalRandom )
import qualified Control.Monad.Mersenne.Random as Random

import Data.IntMap ( IntMap )
import qualified Data.IntMap as IntMap

import System.Random.Mersenne.Pure64    ( newPureMT )


------------------------------------------------------------------------------
import AI.NEAT.Common             ( NeuronId )

import AI.NEAT.Innovations        ( InnovationId )
import AI.NEAT.Innovations.Neuron ( NeuronInnovation )
import AI.NEAT.Innovations.Link   ( LinkInnovation )

import AI.NEAT.Config             ( NEATConfig, defaultNEATConfig )


------------------------------------------------------------------------------
data NEATState =
  NEATState { nextNeuronId      :: !NeuronId
            , nextInnovationId  :: !InnovationId
            , neuronInnovations :: !(IntMap NeuronInnovation)
            , linkInnovations   :: !(IntMap LinkInnovation)
            }


------------------------------------------------------------------------------
emptyNEATState :: NEATState
emptyNEATState = NEATState 0 0 IntMap.empty IntMap.empty


------------------------------------------------------------------------------
newtype NEAT a =
  NEAT { unNEAT :: StateT NEATState (ReaderT NEATConfig Rand) a }
  deriving (Monad,
            MonadReader NEATConfig,
            MonadState NEATState)


------------------------------------------------------------------------------
evalNEAT :: NEAT a -> IO a
evalNEAT (NEAT n) = do
  mt <- newPureMT
  return $ evalRandom (runReaderT (evalStateT n emptyNEATState)
                                  defaultNEATConfig) mt


------------------------------------------------------------------------------
getNeuronId :: NEAT NeuronId
getNeuronId = do
  neuronId <- gets nextNeuronId
  modify (\state -> state { nextNeuronId = neuronId + 1 })

  return neuronId


------------------------------------------------------------------------------
randomR :: (Double, Double) -> NEAT Double
randomR (l, u) =
  NEAT $ do
    value <- lift $ lift $ Random.getDouble
    return $ l + (u - l) * value


------------------------------------------------------------------------------
random :: NEAT Double
random = randomR (-1, 1)
