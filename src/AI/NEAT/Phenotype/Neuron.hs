------------------------------------------------------------------------------
{-# LANGUAGE PatternGuards #-}


------------------------------------------------------------------------------


------------------------------------------------------------------------------
module AI.NEAT.Phenotype.Neuron
       (
         Neuron ( tpy, activationResponse, output ),
         neuron,
         update
       ) where


------------------------------------------------------------------------------
import AI.NEAT.Common        ( NeuronType ( Bias, Input ) )
import AI.NEAT.Genome.Neuron ( NeuronGene )
import qualified AI.NEAT.Genome.Neuron as NeuronGene


------------------------------------------------------------------------------
data Neuron =
  Neuron { tpy                :: !NeuronType
         , activationResponse :: !Double

         , output             :: !Double
         }
  deriving Show

------------------------------------------------------------------------------
-- | Constructs neuron from a gene.
neuron :: NeuronGene -> Neuron
neuron ngene =
  Neuron (NeuronGene.tpy ngene) (NeuronGene.activationResponse ngene) 0


------------------------------------------------------------------------------
-- | Sigmoid function.
sigmoid :: Double               -- ^ Parameter.
        -> Double
        -> Double
sigmoid a x = 1 / (1 + exp (-1 * x / a))


------------------------------------------------------------------------------
-- | Recomputes the output of the neuron.
update :: Neuron                -- ^ Neuron.
       -> Double                -- ^ Input.
       -> Neuron                -- ^ Updated neuron.

-- TODO: excessive update for Bias neuron here
update n input = n { output = output }
  where output | Bias  <- tpy n = -1
               | Input <- tpy n = input
               | otherwise      = sigmoid (activationResponse n) input
