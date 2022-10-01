from bound_infer import TransitionGraph, TransitionBound, DifferenceConstraint, VariableReachingBound
from adapt_search_refined import Graph, AdaptType, AdaptSearchAlgRefined

class AdaptEstimate():
    def __init__(self) -> None:
        pass

    @staticmethod
    def adapt_estimate(unweighted_graph, abs_transition_graph):
        bound_infer = VariableReachingBound(unweighted_graph, abs_transition_graph)
        bound_infer.attach_weights()
        bound_infer.print_weights()

        adapt_search = AdaptSearchAlgRefined(bound_infer.graph)
        adapt_search.search_adapt()
        print("The Adaptivity From This Graph is: ", adapt_search.get_adapt())
