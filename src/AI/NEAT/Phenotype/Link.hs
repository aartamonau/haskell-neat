------------------------------------------------------------------------------


------------------------------------------------------------------------------
module AI.NEAT.Phenotype.Link
       (
         Link ( weight ),
         link
       ) where


------------------------------------------------------------------------------
import AI.NEAT.Genome.Link ( LinkGene )
import qualified AI.NEAT.Genome.Link as LinkGene


------------------------------------------------------------------------------
data Link =
  Link { weight :: !Double
       }
  deriving Show


------------------------------------------------------------------------------
-- | Constructs link from a gene.
link :: LinkGene -> Link
link lgene = Link (LinkGene.weight lgene)
