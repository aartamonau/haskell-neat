------------------------------------------------------------------------------


------------------------------------------------------------------------------
module AI.NEAT.Innovations.Link
       -- TODO: export list
       where


------------------------------------------------------------------------------
import AI.NEAT.Common      ( NeuronId )
import AI.NEAT.Innovations ( InnovationId )


------------------------------------------------------------------------------
data LinkInnovation =
  LinkInnovation { id   :: InnovationId
                 , from :: NeuronId
                 , to   :: NeuronId
                 }
