------------------------------------------------------------------------------
{-# LANGUAGE GeneralizedNewtypeDeriving #-}


------------------------------------------------------------------------------
module AI.NEAT.Monad
       -- TODO: export list
       -- TODO: restructure the code
       where


------------------------------------------------------------------------------
import Control.Applicative              ( Applicative, (<$>) )

import Control.Monad.Trans              ( MonadTrans, lift )

import Control.Monad.Reader             ( MonadReader, asks )
import Control.Monad.Trans.Reader       ( ReaderT, runReaderT )

import Control.Monad.State              ( MonadState, gets, modify )
import Control.Monad.Trans.State.Strict ( StateT, evalStateT )

import Control.Monad.Mersenne.Random    ( Rand (..), R (..), evalRandom )
import qualified Control.Monad.Mersenne.Random as Random


import Data.IntMap ( IntMap )
import qualified Data.IntMap as IntMap

import Data.Map ( Map )
import qualified Data.Map as Map

import System.Random.Mersenne.Pure64    ( newPureMT )


------------------------------------------------------------------------------
import AI.NEAT.Common             ( NeuronId )

import AI.NEAT.Innovations        ( InnovationId )
import AI.NEAT.Innovations.Neuron ( NeuronInnovation ( NeuronInnovation ) )
import AI.NEAT.Innovations.Link   ( LinkInnovation )

import AI.NEAT.Config             ( NEATConfig, defaultNEATConfig )


------------------------------------------------------------------------------
data NEATState =
  NEATState { nextNeuronId      :: !NeuronId
            , nextInnovationId  :: !InnovationId
            , neuronInnovations :: !(Map (NeuronId, NeuronId) NeuronInnovation)
            , linkInnovations   :: !(IntMap LinkInnovation)
            }


------------------------------------------------------------------------------
emptyNEATState :: NEATState
emptyNEATState = NEATState 0 0 Map.empty IntMap.empty


------------------------------------------------------------------------------
newtype NEAT a =
  NEAT { unNEAT :: StateT NEATState (ReaderT NEATConfig Rand) a }
  deriving (Monad,
            Functor,
            Applicative,
            MonadReader NEATConfig,
            MonadState NEATState)


instance Functor Rand where
  fmap f (Rand r) = Rand (\mt -> fmapR f (r mt))
    where fmapR f (R x mt) = R (f x) mt


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
neuronsCount :: NEAT Int
neuronsCount = gets nextNeuronId


------------------------------------------------------------------------------
getInnovationId :: NEAT InnovationId
getInnovationId = do
  innovationId <- gets nextInnovationId
  modify (\state -> state { nextInnovationId = innovationId + 1 })

  return innovationId


------------------------------------------------------------------------------
randomR :: (Double, Double) -> NEAT Double
randomR (l, u) =
  NEAT $ do
    value <- lift $ lift $ Random.getDouble
    return $ l + (u - l) * value


------------------------------------------------------------------------------
random :: NEAT Double
random = randomR (-1, 1)


------------------------------------------------------------------------------
randomInt :: NEAT Int
randomInt =
  NEAT $ lift $ lift $ Random.getInt


------------------------------------------------------------------------------
randomIntR :: Int -> NEAT Int
randomIntR r = fmap (`mod` r) randomInt


------------------------------------------------------------------------------
diceRoll :: (NEATConfig -> Double) -> NEAT a -> NEAT a -> NEAT a
diceRoll rate failure success = do
  rateValue <- asks rate
  r         <- randomR (0, 1)

  if r <= rateValue
    then success
    else failure


------------------------------------------------------------------------------
-- | Finds neuron innovations;
findNeuronInnovation :: (NeuronId, NeuronId)
                     -> NEAT (Maybe NeuronInnovation)
findNeuronInnovation edge = Map.lookup edge <$> gets neuronInnovations


------------------------------------------------------------------------------
-- | Creates new neuron innovation.
createNeuronInnovation :: (NeuronId, NeuronId) -- ^ Edge that is split by new
                                               -- neuron.
                       -> NeuronId             -- ^ Id of new neuron.
                       -> NEAT NeuronInnovation
createNeuronInnovation edge@(src, dst) neuron = do
  id <- getInnovationId
  let innovation = NeuronInnovation id neuron src dst

  newDb <- Map.insert edge innovation <$> gets neuronInnovations
  modify (\s -> s { neuronInnovations = newDb })

  return innovation
