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
         modifyEdges
       ) where


------------------------------------------------------------------------------
import Data.Graph.Inductive ( DynGraph, Node, (&), match, out' )


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
