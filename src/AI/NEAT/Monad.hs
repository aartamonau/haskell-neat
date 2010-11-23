------------------------------------------------------------------------------
{-# LANGUAGE GeneralizedNewtypeDeriving #-}


------------------------------------------------------------------------------
module AI.NEAT.Monad
       -- TODO: export list
       -- TODO: restructure the code
       where


------------------------------------------------------------------------------
import Control.Applicative              ( Applicative, (<$>) )

import Control.Monad                    ( join )
import Control.Monad.Trans              ( MonadTrans, lift )

import Control.Monad.Reader             ( MonadReader, asks )
import Control.Monad.Trans.Reader       ( ReaderT, runReaderT )

import Control.Monad.State              ( MonadState, gets, modify )
import Control.Monad.Trans.State.Strict ( StateT, evalStateT )

import Control.Monad.Mersenne.Random    ( Rand (..), R (..), evalRandom )
import qualified Control.Monad.Mersenne.Random as Random


import Data.Map ( Map )
import qualified Data.Map as Map

import Data.Sequence ( Seq, (><) )
import qualified Data.Sequence as Seq

import System.Random.Mersenne.Pure64    ( newPureMT )


------------------------------------------------------------------------------
import AI.NEAT.Common             ( NeuronId )

import AI.NEAT.Innovations        ( InnovationId )
import AI.NEAT.Innovations.Neuron ( NeuronInnovation ( NeuronInnovation ) )
import AI.NEAT.Innovations.Link   ( LinkInnovation ( LinkInnovation ) )

import AI.NEAT.Config             ( NEATConfig, defaultNEATConfig )


------------------------------------------------------------------------------
-- | Type synonym for DB of neuron innovations.
type NeuronInnovationDB = Map (NeuronId, NeuronId) (Seq NeuronInnovation)


------------------------------------------------------------------------------
-- | Type synonym for DB of link innovations.
type LinkInnovationDB = Map (NeuronId, NeuronId) LinkInnovation


------------------------------------------------------------------------------
data NEATState =
  NEATState { nextNeuronId      :: !NeuronId
            , nextInnovationId  :: !InnovationId
            , neuronInnovations :: !NeuronInnovationDB
            , linkInnovations   :: !LinkInnovationDB
            }


------------------------------------------------------------------------------
emptyNEATState :: NEATState
emptyNEATState = NEATState 0 0 Map.empty Map.empty


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
-- | Randomly selects one of two objects.
randomChoice :: a -> a -> NEAT a
randomChoice x y = do
  r <- randomR (0, 1)
  if r <= 0.5
    then return x
    else return y

------------------------------------------------------------------------------
diceRoll :: (NEATConfig -> Double) -> NEAT a -> NEAT a -> NEAT a
diceRoll rate failure success = do
  rateValue <- asks rate
  r         <- randomR (0, 1)

  if r <= rateValue
    then success
    else failure


------------------------------------------------------------------------------
-- | Finds first in historical order neuron innovation for some edge that
-- matches provided predicate.
findNeuronInnovation :: (NeuronId, NeuronId)       -- ^ Edge vertexes.
                     -> (NeuronInnovation -> Bool) -- ^ Neuron predicate.
                     -> NEAT (Maybe NeuronInnovation)
findNeuronInnovation edge p = do
  seq <- Map.lookup edge <$> gets neuronInnovations
  return $ join $ fmap find seq

  where -- TODO: something better?
        find seq = Seq.index seq <$> Seq.findIndexL p seq


------------------------------------------------------------------------------
-- | Creates new neuron innovation.
createNeuronInnovation :: (NeuronId, NeuronId) -- ^ Edge that is split by new
                                               -- neuron.
                       -> NeuronId             -- ^ Id of new neuron.
                       -> NEAT NeuronInnovation
-- TODO: data-accessor
createNeuronInnovation edge@(src, dst) neuron = do
  db <- gets neuronInnovations
  let updatedDb = Map.insertWith (><) edge (Seq.singleton innovation) db

  modify (\s -> s { neuronInnovations = updatedDb })
  return innovation

  where innovation = NeuronInnovation neuron src dst


------------------------------------------------------------------------------
-- | Finds link innovation.
findLinkInnovation :: (NeuronId, NeuronId)
                   -> NEAT (Maybe LinkInnovation)
findLinkInnovation edge = Map.lookup edge <$> gets linkInnovations


------------------------------------------------------------------------------
-- | Creates link innovation.
createLinkInnovation :: (NeuronId, NeuronId) -- ^ Edge.
                     -> NEAT LinkInnovation
createLinkInnovation edge@(src, dst) = do
  innoId <- getInnovationId
  db     <- gets linkInnovations

  let innovation = LinkInnovation innoId src dst
  let updatedDb  = Map.insert edge innovation db

  modify (\s -> s { linkInnovations = updatedDb })
  return innovation


------------------------------------------------------------------------------
-- | Checks whether certain link innovation exists. If it does not then new
-- one is created.
findOrCreateLinkInnovation :: (NeuronId, NeuronId)
                           -> NEAT LinkInnovation
findOrCreateLinkInnovation edge = do
  mInnovation <- findLinkInnovation edge

  case mInnovation of
    Nothing         -> createLinkInnovation edge
    Just innovation -> return innovation
