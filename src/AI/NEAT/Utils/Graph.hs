------------------------------------------------------------------------------
-- |
-- Module      : AI.NEAT.Utils.Graph
-- Copyright   : (C) 2010 Aliaksiej Artamonaŭ
-- License     : LGPL
--
-- Maintainer  : aliaksiej.artamonau@gmail.com
-- Stability   : unstable
-- Portability : unportable
--
-- Various utility functions to work with graphs from FGL library.
--
------------------------------------------------------------------------------



------------------------------------------------------------------------------
module AI.NEAT.Utils.Graph
       (
         -- * Miscellaneous
         modifyEdges,

         -- * Monadic computations
         --
         -- ** Sequencing monadic computations
         --
         --    $sequencingNote
         sequenceGrN,
         sequenceGrE,
         sequenceGr,

         -- ** Monadic maps
         nmapM,
         emapM
       ) where


------------------------------------------------------------------------------
import Data.Graph.Inductive ( DynGraph, Node, (&),
                              match, out', nmap, emap, ufold, empty )


------------------------------------------------------------------------------
-- | Modifies labels of all the edges between two nodes.
modifyEdges :: DynGraph gr
            => (Node, Node)     -- ^ Edges.
            -> (b -> b)         -- ^ Function to map labels through.
            -> gr a b           -- ^ Graph.
            -> gr a b
modifyEdges (src, dst) f g = maybe g (& g') newContext
  where (context, g') = match src g
        newContext    = fmap modifyLinks context

        modifyLinks (is, n, nl, os) = (is, n, nl,
                                       map (\(l, n) -> if n == dst
                                                       then (f l, n)
                                                       else (  l, n))
                                           os)


------------------------------------------------------------------------------
-- $sequencingNote
-- There is no well defined order in which sequencing of monadic actions is
-- performed by the following functions. So those computations must not depend
-- on the order.
------------------------------------------------------------------------------
-- | Evaluates monadic computations stored in nodes' labels.
sequenceGrN :: (Monad m, DynGraph gr)
            => gr (m a) b
            -> m (gr a b)
sequenceGrN = ufold k (return empty)
  where k (ins, n, l, outs) rest = do
          l'    <- l
          rest' <- rest

          return $ (ins, n, l', outs) & rest'


------------------------------------------------------------------------------
-- | Evaluates monadic computations stored in edges' labels.
sequenceGrE :: (Monad m, DynGraph gr)
            => gr a (m b)
            -> m (gr a b)
sequenceGrE = ufold k (return empty)
  where k (ins, n, l, outs) rest = do
          ins'  <- seqAdjs ins
          outs' <- seqAdjs outs
          rest' <- rest

          return $ (ins', n, l, outs') & rest'

        seqAdjs xs = do
          let ns = map snd xs
          ls    <- sequence (map fst xs)

          return $ zip ls ns

------------------------------------------------------------------------------
-- | Evaluates monadic computations from both nodes' and edges' labels.
sequenceGr :: (Monad m, DynGraph gr)
           => gr (m a) (m b)
           -> m (gr a b)
sequenceGr g = sequenceGrN g >>= sequenceGrE


------------------------------------------------------------------------------
-- | Maps monadic action over the labels of graph nodes.
nmapM :: (Monad m, DynGraph gr)
      => (a -> m c)
      -> gr a b
      -> m (gr c b)
nmapM k g = sequenceGrN (nmap k g)


------------------------------------------------------------------------------
-- | Maps monadic action over the labels of graph edges.
emapM :: (Monad m, DynGraph gr)
      => (b -> m c)
      -> gr a b
      -> m (gr a c)
emapM k g = sequenceGrE (emap k g)
