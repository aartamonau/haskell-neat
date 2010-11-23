------------------------------------------------------------------------------


------------------------------------------------------------------------------
module AI.NEAT.Genome.Link
       -- TODO: export list
       where


------------------------------------------------------------------------------
import Data.Graph.Inductive ( LEdge )


------------------------------------------------------------------------------
import AI.NEAT.Common ( NeuronId )
import AI.NEAT.Monad ( NEAT, findOrCreateLinkInnovation )

import AI.NEAT.Genome.Neuron ( NeuronGene )
import qualified AI.NEAT.Genome.Neuron as Neuron

import AI.NEAT.Innovations.Link ( LinkInnovation )
import qualified AI.NEAT.Innovations.Link as LInnovation


------------------------------------------------------------------------------
-- TODO: Innovations
-- TODO: Maybe it's sensible to embed neuron genes here (to avoid auxiliarry
--       queries to graph)
data LinkGene =
  LinkGene { from      :: !NeuronId
           , to        :: !NeuronId
           , weight    :: !Double
           , isEnabled :: !Bool
           }


instance Show LinkGene where
  show = show . weight


------------------------------------------------------------------------------
-- | Creates link gene and corresponding innovation.
linkGene :: NeuronGene -> NeuronGene -> Double -> NEAT LinkGene
linkGene from to weight = do
  let link = LinkGene (Neuron.id from) (Neuron.id to) weight True

  _ <- findOrCreateLinkInnovation (Neuron.id from, Neuron.id to)

  return link


------------------------------------------------------------------------------
-- | Creates link gene based on the info stored in link innovation.
linkGene_ :: LinkInnovation -> Double -> LinkGene
linkGene_ inno weight =
  LinkGene (LInnovation.from inno) (LInnovation.to inno) weight True


------------------------------------------------------------------------------
-- | Determines whether a link is looped.
isLooped :: LinkGene -> Bool
isLooped link = (from link) == (to link)


------------------------------------------------------------------------------
toLEdge :: LinkGene -> LEdge LinkGene
toLEdge link = (from link, to link, link)
