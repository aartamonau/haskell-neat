------------------------------------------------------------------------------


------------------------------------------------------------------------------
module AI.NEAT.Innovations.Neuron
       -- TODO: export list
       where


------------------------------------------------------------------------------
import AI.NEAT.Common      ( NeuronId )
import AI.NEAT.Innovations ( InnovationId )


------------------------------------------------------------------------------
data NeuronInnovation =
  NeuronInnovation { id       :: InnovationId
                   , neuronId :: NeuronId
                   , from     :: NeuronId
                   , to       :: NeuronId
                   }
