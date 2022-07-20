import math
from bound_infer import TransitionGraph, TransitionBound, DifferenceConstraint
# , VariableReachingBound
from adapt_search_refined import Graph, AdaptType, AdaptSearchAlgRefined

class AdaptEstimate():
    def __init__(self) -> None:
        pass


    # @staticmethod
    class VariableReachingBound():
        def __init__(self, graph=Graph(), transition_graph=TransitionGraph()) -> None:
            self.graph = graph
            self.transition_graph = transition_graph
        
        def attach_weights(self):
            transition_bounds = TransitionBound(self.transition_graph).compute_transition_bounds()
            for (t_index, b) in enumerate(transition_bounds):
                transition = self.transition_graph.transitions[t_index]
                for var_vertex in transition[3] :
                    self.graph.weights[var_vertex] = self.graph.weights[var_vertex] + AdaptType(b)
        
        def get_weights(self):
            return [w.value for w in self.graph.weights]
        
        def print_weights(self):
            for transition in self.transition_graph.transitions:
                for var_vertex in transition[3]:
                    if not transition[1] == []:
                    #     print( "weight for a testing command of label : " + str(var_vertex) + " is: " + str(self.graph.weights[var_vertex].value))
                    # else:
                        print( "weight for Variable: " + transition[1][0].get_var() + " of label " + str(var_vertex) + " is: " + str(self.graph.weights[var_vertex].value))

    @staticmethod
    def adapt_estimate(unweighted_graph, abs_transition_graph):
        bound_infer = AdaptEstimate.VariableReachingBound(unweighted_graph, abs_transition_graph)
        bound_infer.attach_weights()
        bound_infer.print_weights()

        adapt_search = AdaptSearchAlgRefined(bound_infer.graph)
        adapt_search.search_adapt()
        adapt_search.print_adapt()
        # print("The Adaptivity From This Graph is: ", adapt_search.get_adapt())
        # print("The Total Query Number For This Graph is: ", sum(unweighted_graph.query))
        # print("The Estimated Generalization Error with an Optimial qurey computation Mechanism is O( ", 
        # adapt_search.get_adapt() * AdaptType(str(math.sqrt(sum(unweighted_graph.query)))), "/ (n) )")
        
