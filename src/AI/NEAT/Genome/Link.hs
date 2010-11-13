------------------------------------------------------------------------------


------------------------------------------------------------------------------
module AI.NEAT.Genome.Link
       -- TODO: export list
       where


------------------------------------------------------------------------------
import AI.NEAT.Common ( NeuronId )
import AI.NEAT.Monad ( NEAT )


------------------------------------------------------------------------------
-- TODO: Innovations
data LinkGene =
  LinkGene { from       :: !NeuronId
           , to         :: !NeuronId
           , weight     :: !Double
           , enabled    :: !Bool
           }


instance Show LinkGene where
  show = show . weight


------------------------------------------------------------------------------
-- TODO: innovations
linkGene :: NeuronId -> NeuronId -> Double -> NEAT LinkGene
linkGene from to weight = return $ LinkGene from to weight True
