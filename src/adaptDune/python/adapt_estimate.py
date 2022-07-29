from bound_infer import TransitionGraph, TransitionBound
from adapt_search_refined import Graph, AdaptType, AdaptSearchAlgRefined

class AdaptEstimate():
    def __init__(self) -> None:
        pass


    # @staticmethod
    class ProgramBasedDependencyGraphWeightsEstimation():
        def __init__(self, graph=Graph(), transition_graph=TransitionGraph()) -> None:
            self.graph = graph
            self.transition_graph = transition_graph

        def vertex_weights_estimate(self):
            transition_bounds = TransitionBound(self.transition_graph).compute_transition_bounds()
            for (t_index, b) in enumerate(transition_bounds):
                transition = self.transition_graph.transitions[t_index]
                for var_vertex in transition[3] :
                    self.graph.weights[var_vertex] = self.graph.weights[var_vertex] + AdaptType(b)
        
        def get_vertex_weights(self):
            return [w.value for w in self.graph.weights]
        
        def print_vertex_weights(self):
            for (_, dc_set, _, var_set) in self.transition_graph.transitions:
                for var in var_set:
                    print( "weight for Variable or Bool guard: " 
                        + str(dc_set[0].get_var() if dc_set[0].get_var() else dc_set[0].dc_bexpr) + " of label " 
                        + str(var) + " is: " + str(self.graph.weights[var].value))

        def edge_weights_estimate(self):
            transition_bounds = TransitionBound(self.transition_graph).compute_transition_bounds()
            # Edge Weight Initialize
            for (t_index, b) in enumerate(transition_bounds):
                transition = self.transition_graph.transitions[t_index]
                edge = str(transition[0]) + "->" + str(transition[-2])
                self.graph.edge_weights[edge] = (self.graph.edge_weights[edge].adapt_max(AdaptType(b))) if (edge in self.graph.edge_weights.keys()) else AdaptType(b)
            # Edge Weight DFS
            for (u, v) in self.graph.edges:
                edge = str(u) + "->" + str(v)
                paths = self.transition_graph.search_path(u, v)
                for p in paths:
                    p_reachability_bounds = AdaptType(0)
                    for i in p: p_reachability_bounds = p_reachability_bounds.adapt_min(AdaptType(transition_bounds[i]))
                    self.graph.edge_weights[edge] = (self.graph.edge_weights[edge].adapt_max(p_reachability_bounds))

       
        def get_edge_weights(self):
            return self.graph.edge_weights.items()
        
        def print_edge_weights(self):
            for s in ["weight for Edge: " + w[0] + " is: " + str(w[1].value) for w in self.graph.edge_weights.items()]:
                print( s )


        def attach_weights(self):
            transition_bounds = TransitionBound(self.transition_graph).compute_transition_bounds()
            for (t_index, b) in enumerate(transition_bounds):
                transition = self.transition_graph.transitions[t_index]
                for var_vertex in transition[3] :
                    self.graph.weights[var_vertex] = self.graph.weights[var_vertex] + AdaptType(b)
            
            self.edge_weights_estimate()
            self.vertex_weights_estimate()

        def print_weights(self):
            self.print_vertex_weights()
            self.print_edge_weights()
    # @staticmethod

    @staticmethod
    def adapt_estimate(unweighted_graph, abs_transition_graph):
        weight_infer = AdaptEstimate.ProgramBasedDependencyGraphWeightsEstimation(unweighted_graph, abs_transition_graph)
        weight_infer.edge_weights_estimate()
        weight_infer.attach_weights()
        weight_infer.print_weights()

        adapt_search = AdaptSearchAlgRefined(weight_infer.graph)
        adapt_search.search_adapt()
        adapt_search.print_adapt()        
