from collections import defaultdict
import time
from abstract_transition_graph import TransitionGraph, DifferenceConstraint
from bound_infer import TransitionBound
from adapt_lib import AdaptType, Graph
from adapt_search import AdaptSearchAlgRefined
from program_refine import ProgramRefine
from rechability_bound_pathsensitive import PathSensitiveReachabilityBound

class AdaptEstimate():
    def __init__(self) -> None:
        pass


    # @staticmethod
    class ProgramBasedDependencyGraphWeightsEstimation():
        def __init__(self, graph=Graph(), transition_graph=TransitionGraph()) -> None:
            self.graph = graph
            self.transition_graph = transition_graph
            self.reachability_bound = defaultdict(AdaptType)

        def reachability_bound_estimation(self):
            # Path-Insensitive Version:
            self.reachability_bound = TransitionBound(self.transition_graph).compute_transition_bounds()

            # # Path-Sensitive Version:
            # reachability_bound_path = PathSensitiveReachabilityBound(self.transition_graph).compute_rb(ProgramRefine(self.transition_graph).get_result())
            # for transition_path, bound in reachability_bound_path.items():
            #     for transition_id in [int(id) for id in (transition_path[1:-1].split(", "))]:
            #         self.reachability_bound[transition_id] = bound
            return

        def vertex_weights_estimate(self):
            self.graph.weights = [AdaptType(0)]*self.graph.get_vertice_num()
            for (t_index, b) in enumerate(self.reachability_bound):
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
            # Edge Weight Initialize
            # These are Weights for Edges corresponding to the Control Flow, not necessary the data denepdency edge
            for (t_index, b) in enumerate(self.reachability_bound):
                transition = self.transition_graph.transitions[t_index]
                edge = str(transition[0]) + "->" + str(transition[-2])
                self.graph.edge_weights[edge] = (self.graph.edge_weights[edge].adapt_max(AdaptType(b))) if (edge in self.graph.edge_weights.keys()) else AdaptType(b)
            # Edge Weight DFS
            # Compute the Weight for Edges in Data Dependency Graph, Every Edge on this graph should correspond to
            # a path on the Control Flow Graph
            for (u, v) in self.graph.edges:
                edge = str(u) + "->" + str(v)
                paths = self.transition_graph.search_path(u, v)
                for p in paths:
                    p_reachability_bounds = AdaptType(0)
                    for i in p: p_reachability_bounds = p_reachability_bounds.adapt_min(AdaptType(self.reachability_bound[i]))
                    self.graph.edge_weights[edge] = (self.graph.edge_weights[edge].adapt_max(p_reachability_bounds))

       
        def get_edge_weights(self):
            return self.graph.edge_weights.items()
        
        def print_edge_weights(self):
            for s in ["weight for Edge: " + w[0] + " is: " + str(w[1].value) for w in self.graph.edge_weights.items()]:
                print( s )
        
        def weight_estimate(self):
            self.reachability_bound_estimation()
            self.edge_weights_estimate()
            self.vertex_weights_estimate()


        def weight_estimate2(self):
            for i in range(self.graph.get_vertice_num()):
                self.graph.weights[i] = AdaptType(1)

        def print_weights(self):
            self.print_vertex_weights()
            self.print_edge_weights()
    # @staticmethod

    @staticmethod
    def adapt_estimate(dcf_graph, abs_transition_graph):
        start_time = time.time()
        weight_infer = AdaptEstimate.ProgramBasedDependencyGraphWeightsEstimation(dcf_graph, abs_transition_graph)
        if dcf_graph.weights is None:
            weight_infer.weight_estimate()
            print("--- REACHABILITY BOUND COMPUTATION TIME: %s seconds ---" % (time.time() - start_time))
        else:
            print("--- REACHABILITY BOUNDS ARE PARSED FROM FILE: %s seconds ---" % (time.time() - start_time))
        # weight_infer.print_weights()

        adapt_search = AdaptSearchAlgRefined(weight_infer.graph)
        start_time = time.time()
        adapt_search.search_adapt()
        print("--- ADAPTIVITY COMPUTATION TIME: %s seconds ---" % (time.time() - start_time))
        adapt_search.print_adapt()        
