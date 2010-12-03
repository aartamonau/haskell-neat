------------------------------------------------------------------------------
{-# LANGUAGE ViewPatterns #-}


------------------------------------------------------------------------------
module AI.NEAT.Species
       (
         Species,
         speciate
       ) where


------------------------------------------------------------------------------
import Control.Exception    ( assert )
import Control.Monad.Reader ( asks )
import Data.Sequence        ( Seq, ViewL (EmptyL, (:<)), (|>), (><),
                              singleton, empty )
import qualified Data.Sequence as Seq

import qualified AI.NEAT.Config as Config
import AI.NEAT.Genome ( Genome, compatibility, fitness )
import AI.NEAT.Monad  ( NEAT )


------------------------------------------------------------------------------
data Species =
  Species { bestFitness    :: Double
          , averageFitness :: Double
          , age            :: Int

          -- TODO: overkill?
          , genomes        :: Seq Genome
          }


------------------------------------------------------------------------------
-- | Creates a species containing a single genome.
species :: Genome -> Species
species genome = Species (fitness genome) (fitness genome) 0 (singleton genome)


------------------------------------------------------------------------------
-- | Conses a genome into a species.
--
-- TODO: bestFitness, averageFitness
cons :: Species -> Genome -> Species
cons s g = s { genomes = gs |> g }
  where gs = genomes s


------------------------------------------------------------------------------
-- | Returns the genome for which has been created.
first :: Species -> Genome
first (genomes -> gs) = assert (not $ Seq.null gs) (Seq.index gs 0)


------------------------------------------------------------------------------
-- | Splits genomes into species according to their compatibility distance.
speciate :: [Genome]  -- ^ Genome with its fitness value.
         -> NEAT (Seq Species)
speciate gs = whole gs empty
  where similar :: Species -> Genome -> NEAT Bool
        similar s g = do
          threshold <- asks Config.speciesCompatThreshold
          compat    <- compatibility g (first s)

          if compat < threshold
            then return True
            else return False

        single :: Genome -> Seq Species -> NEAT (Seq Species)
        single g ss = go ss empty
          where go (Seq.viewl -> EmptyL)  rs = return $ rs |> species g
                go (Seq.viewl -> s :< ss) rs = do
                  similar_ <- similar s g
                  if similar_
                    then return $ (rs |> cons s g) >< ss
                    else go ss (rs |> s)

        whole :: [Genome] -> Seq Species -> NEAT (Seq Species)
        whole []       ss = return ss
        whole (g : gs) ss = do
          ss_ <- single g ss
          whole gs ss_
