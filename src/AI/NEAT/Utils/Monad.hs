------------------------------------------------------------------------------
-- |
-- Module      : AI.NEAT.Utils.Monad
-- Copyright   : (C) 2010 Aliaksiej Artamonaŭ
-- License     : LGPL
--
-- Maintainer  : aliaksiej.artamonau@gmail.com
-- Stability   : unstable
-- Portability : unportable
--
-- Various monadic utility functions.
--
------------------------------------------------------------------------------


------------------------------------------------------------------------------
module AI.NEAT.Utils.Monad
       (
         matching,
         matchingM,
         matchingTries,
         matchingTriesM
       ) where


------------------------------------------------------------------------------
-- | Performs monadic action until it matches a predicate.
matchingM :: Monad m
          => m a                 -- ^ Action.
          -> (a -> m Bool)       -- ^ Predicate.
          -> m a
matchingM a p = go
  where go = do
          a' <- a
          r  <- p a'
          if r
            then return a'
            else go


------------------------------------------------------------------------------
-- | Version of 'matchingM' taking non-monadic predicate.
matching :: Monad m
         => m a
         -> (a -> Bool)
         -> m a
matching a p = matchingM a (return . p)


------------------------------------------------------------------------------
-- | Performs monadic action until it matches a predicate. Stops after
-- specified number of unsuccessfull attempts.
matchingTriesM :: Monad m
               => m a             -- ^ Action.
               -> (a -> m Bool)   -- ^ Predicate.
               -> Int             -- ^ Number of attempts.
               -> m (Maybe a)
matchingTriesM a p tries = go tries
  where go 0 = return Nothing
        go n = do
          a' <- a
          r  <- p a'
          if r
            then return (Just a')
            else go (n - 1)


------------------------------------------------------------------------------
-- | Version of 'matchingTriesM' taking non-monadic predicate.
matchingTries :: Monad m
              => m a
              -> (a -> Bool)
              -> Int
              -> m (Maybe a)
matchingTries a p = matchingTriesM a (return . p)
