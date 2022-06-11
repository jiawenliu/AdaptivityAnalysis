from bound_infer import TransitionGraph, TransitionBound, DifferenceConstraint, VariableReachingBound
from adapt_search_refined import Graph, AdaptType, AdaptSearchAlgRefined
from adapt_estimate import AdaptEstimate
from graph_parse import GraphParser
class TestUnits:

    def __init__(self, ALG) -> None:
        self.ALG = ALG

    # the example with Simple sequence, 
    # Expected Weights: [1,1,1,1]
    # Expected Adaptivity: 4
    def seq(self):
        query = [1, 1, 1, 1]
        edges = [(0, 1), (1, 2), (2, 3)]
        weights = [AdaptType("INF")]*len(query)

        # adapt_search = self.ALG(Graph(edges, weights, query))

        ctl_edges = [(2, 3), (1, 2), (0, 1), (3, 4)]
        transitions = [
            (2, [ DifferenceConstraint("z", None, "Q", DifferenceConstraint.DCType.RESET) ], 3, [2]),
            (1, [ DifferenceConstraint("y", None, "Q", DifferenceConstraint.DCType.RESET) ], 2, [1]),
            (0, [ DifferenceConstraint("x", None, "Q", DifferenceConstraint.DCType.RESET) ], 1, [0]),
            (3, [ DifferenceConstraint("w", None, "Q", DifferenceConstraint.DCType.RESET) ], -1, [3])
            ]
        AdaptEstimate.adapt_estimate(Graph(edges, weights, query), TransitionGraph(ctl_edges, transitions))
        print("The Reachability Bounds Expected for  Vertices in Pure Sequence Graph are: [1,1,1,1] ")
        print("The Adaptivity Expected for Simple Seq Algorithm is: 4 ")


    # the example with Simple sequence, 
    # Expected Weights: [1,1,1,1]
    # Expected Adaptivity: 4
    def seq_multivar(self):
        query = [1, 1, 1, 1]
        edges = [(0, 1), (0, 2), (1, 2), (1, 3), (2, 3)]
        weights = [AdaptType("INF")]*len(query)

        # adapt_search = self.ALG(Graph(edges, weights, query))

        ctl_edges = [(2, 3), (1, 2), (0, 1), (3, 4)]
        transitions = [
            (2, [ DifferenceConstraint("z", None, "Q", DifferenceConstraint.DCType.RESET) ], 3, [2]),
            (1, [ DifferenceConstraint("y", None, "Q", DifferenceConstraint.DCType.RESET) ], 2, [1]),
            (0, [ DifferenceConstraint("x", None, "Q", DifferenceConstraint.DCType.RESET) ], 1, [0]),
            (3, [ DifferenceConstraint("w", None, "Q", DifferenceConstraint.DCType.RESET) ], -1, [3])
            ]
        AdaptEstimate.adapt_estimate(Graph(edges, weights, query), TransitionGraph(ctl_edges, transitions))
        print("The Reachability Bounds Expected for  Vertices in Pure Sequence Graph are: [1,1,1,1] ")
        print("The Adaptivity Expected for Simple Seq Algorithm is: 4 ")

    # the example in if branch and only value dependency, 
    # Expected Adaptivity: 2
    # Ouput Adaptivity: 2
    def if_valdep(self):
        query = [1, 0, 1, 1]
        edges = [(0, 2), (1, 2), (1, 3)]
        weights = [AdaptType("INF")]*len(query)

        ctl_edges = [(0, 1), (1, 2), (2, 3), (2, 4), (3, 5), (4, 5)]
        transitions = [
            (0, [DifferenceConstraint("x", None, "Q", DifferenceConstraint.DCType.RESET)], 1, [0]),
            (0, [DifferenceConstraint("z", None, "Q", DifferenceConstraint.DCType.RESET)], 1, [1]),
            (0, [], 1, []),
            (0, [], 1, []),
            (0, [DifferenceConstraint("y", None, "Q", DifferenceConstraint.DCType.RESET)], 1, [2]),
            (0, [DifferenceConstraint("y", None, "Q", DifferenceConstraint.DCType.RESET)], 1, [3]),
            ]


        AdaptEstimate.adapt_estimate(Graph(edges, weights, query), TransitionGraph(ctl_edges, transitions))
        print("The Reachability Bounds Expected for Mutli-path Dependency in If branch Algorithm are: [1,1,1,1] ")

        # adapt_search = AdaptSearchAlgRefined(bound_infer.graph)
        # adapt_search.search_adapt()
        print("The Adaptivity Expected for Mutli-path Dependency in If branch Algorithm is: 2 ")


    def if_ctldep(self):
        query = [1, 1, 1, 1]
        edges = [(0, 1), (1, 2), (1, 3)]
        weights = [AdaptType("INF")]*len(query)

        ctl_edges = [(0, 1), (1, 2), (2, 3), (2, 4), (3, 5), (4, 5)]
        transitions = [
            (0, [DifferenceConstraint("x", None, "Q", DifferenceConstraint.DCType.RESET)], 1, [0]),
            (0, [DifferenceConstraint("z", None, "Q", DifferenceConstraint.DCType.RESET)], 1, [1]),
            (0, [], 1, []),
            (0, [], 1, []),
            (0, [DifferenceConstraint("y", None, "Q", DifferenceConstraint.DCType.RESET)], 1, [2]),
            (0, [DifferenceConstraint("w", None, "Q", DifferenceConstraint.DCType.RESET)], 1, [3]),
            ]

        AdaptEstimate.adapt_estimate(Graph(edges, weights, query), TransitionGraph(ctl_edges, transitions))

        # bound_infer = self.ALG(Graph(edges, weights, query), TransitionGraph(ctl_edges, transitions))
        # bound_infer.attach_weights()
        print("The Reachability Bounds Expected for  Vertices in If Branch with Control Dependency Graph are: [1, 1, 1, 1] ")
        print("The Adaptivity Expected for Mutli-path Dependency in If branch Algorithm is: 3 ")

    # the example with simple Loop sequence, 
    # Expected Weights: [1, 1, 100, 100, 100]
    def while_valdep(self):
        query = [0, 0, 1, 0, 0]
        edges = [(0, 2), (0, 3), (0, 4), (1, 2), (1, 3), (2, 3), (3, 2), (3, 3), (4, 4)]
        weights = [AdaptType("INF")]*len(query)

        ctl_edges = [(4, 5), (3, 4), (2, 3), (5, 2), (1, 2), (0, 1), (2, 7)]

        transitions = [
            (4, [ DifferenceConstraint("l", None, "INF", DifferenceConstraint.DCType.RESET) ], 5, [3]),
            (3, [ DifferenceConstraint("a", None, "Q", DifferenceConstraint.DCType.RESET) ], 4, [2]),
            (2, [], 3, []),
            (5, [ DifferenceConstraint("i", None, " 1 ", DifferenceConstraint.DCType.DEC) ], 2, [4]),
            (1, [ DifferenceConstraint("l", None, " 0 ", DifferenceConstraint.DCType.RESET) ], 2, [1]),
            (0, [ DifferenceConstraint("i", None, " 100 ", DifferenceConstraint.DCType.RESET) ], 1, [0]),
            (2, [], -1, [])] 
        AdaptEstimate.adapt_estimate(Graph(edges, weights, query), TransitionGraph(ctl_edges, transitions))
        print("The Reachability Bounds Expected for  Vertices in Simple while are: [1, 1, 100, 100, 100] ")
        # print("The Reachability Bounds Calculated for Vertices in This Graph are: ", bound_infer.get_weights())
        
        # adapt_search = AdaptSearchAlgRefined(bound_infer.graph)
        # adapt_search.search_adapt()
        print("The Adaptivity Expected for Simple While Algorithm is: 100 ")

    # the example with multi path Loop sequence, 
    def while_multipath(self):
        query = [0, 1, 0, 1, 1, 1]
        edges = [(0, 2), (0,3), (0,4), (0,5), (2, 2), (1, 3), (1, 4), (1, 5), (3, 5), (5, 3), (5, 4)]
        weights = [AdaptType("INF")]*len(query)

        ctl_edges = [(4, 5), (4, 6), (5, 7), (6, 7), (3, 4), (2, 3), (7, 2), (1, 2), (0, 1), (2, 8)]
        transitions = [
            (4, [], 5, []),
            (4, [], 6, []),
            (5, [ DifferenceConstraint("y", None, "Q", DifferenceConstraint.DCType.RESET) ], 7, [3]),
            (6, [ DifferenceConstraint("w", None, "Q", DifferenceConstraint.DCType.RESET) ], 7, [4]),
            (3, [ DifferenceConstraint("i", None, " 1 ", DifferenceConstraint.DCType.DEC) ], 4, [2]),
            (2, [], 3, []),
            (7, [ DifferenceConstraint("x", None, "Q", DifferenceConstraint.DCType.RESET) ], 2, [5]),
            (1, [ DifferenceConstraint("x", None, "Q", DifferenceConstraint.DCType.RESET) ], 2, [1]),
            (0, [ DifferenceConstraint("i", None," k ", DifferenceConstraint.DCType.RESET) ], 1, [0]),
            (2, [], -1, [])
        ]


        AdaptEstimate.adapt_estimate(Graph(edges, weights, query), TransitionGraph(ctl_edges, transitions))
        print("The Reachability Bounds Expected for  Vertices in while with If Banch inside are: [1, 1, k, k/2, k/2, k] ")

        print("The Adaptivity Expected for while with If Banch (Multi-Path While Loop) Algorithm is: 1 + k/2 + k/2")
        # print("The Adaptivity From This Graph is: ", adapt_search.get_adapt())

    # the example with multi path Loop sequence, 
    def while_multivaldep(self):
        query = [0, 1, 1, 0, 1, 1, 1]
        edges = [(0, 3), (0, 4), (1, 4), (2, 4), (4, 6), (6, 4), (4, 5), (5, 4)]
        weights = [AdaptType("INF")]*len(query)

        ctl_edges = [
            (6, 7), (5, 6), (4, 5), (3, 4), (7, 3), (2, 3), (1, 2), (0, 1), (3, 8)]
        transitions = [
                (6, [ DifferenceConstraint("x", None, "Q", DifferenceConstraint.DCType.RESET) ], 7, [5]),
                (5, [ DifferenceConstraint("y", None, "Q", DifferenceConstraint.DCType.RESET) ], 6, [4]),
                (4, [ DifferenceConstraint("i", None, "1", DifferenceConstraint.DCType.DEC) ], 5, [3]),
                (3, [], 4, []),
                (7, [ DifferenceConstraint("z", None, "Q", DifferenceConstraint.DCType.RESET) ], 3, [6]),
                (2, [ DifferenceConstraint("z", None, "Q", DifferenceConstraint.DCType.RESET) ], 3, [2]),
                (1, [ DifferenceConstraint("x", None, "Q", DifferenceConstraint.DCType.RESET) ], 2, [1]),
                (0, [ DifferenceConstraint("i", None, "k", DifferenceConstraint.DCType.RESET) ], 1, [0]),
                (3, [], -1, [])
        ]


        AdaptEstimate.adapt_estimate(Graph(edges, weights, query), TransitionGraph(ctl_edges, transitions))
        print("The Reachability Bounds Expected for Vertices in while Multi Variable Dependency: [1, 1, 1, k, k, k, k] ")

        print("The Adaptivity Expected for while with (Multi Variable Dependency) Algorithm is: 1 + 2 * k")
        # print("The Adaptivity From This Graph is: ", adapt_search.get_adapt())

    # the example with simple Loop sequence, 
    # Expected Weights: [1, 1, INF, INF]
    def while_valctldep(self):
        query = [1, 1, 1, 1]
        edges = [(0, 2), (0, 3), (1, 2), (2, 3), (3, 2)]
        weights = [AdaptType("INF")]*len(query)

        ctl_edges = [(3, 4), (2, 3), (4, 2), (1, 2), (0, 1), (2, 5)]

        transitions = [
            (3, [ DifferenceConstraint("x", None, "Q", DifferenceConstraint.DCType.RESET) ], 4, [2]),
            (2, [], 3, []),
            (4, [ DifferenceConstraint("z", None, "Q", DifferenceConstraint.DCType.RESET) ], 2, [3]),
            (1, [ DifferenceConstraint("z", None, "Q", DifferenceConstraint.DCType.RESET) ], 2, [1]),
            (0, [ DifferenceConstraint("x", None, "Q", DifferenceConstraint.DCType.RESET) ], 1, [0]),
            (2, [], -1, [])] 
        AdaptEstimate.adapt_estimate(Graph(edges, weights, query), TransitionGraph(ctl_edges, transitions))
        print("The Reachability Bounds Expected for  Vertices in While with Control and Value Dependency Overlapped are: [1, 1, INF, INF] ")
        # print("The Reachability Bounds Calculated for Vertices in This Graph are: ", bound_infer.get_weights())
        
        # adapt_search = AdaptSearchAlgRefined(bound_infer.graph)
        # adapt_search.search_adapt()
        print("The Adaptivity Expected for While with Control and Value Dependency Overlapped Algorithm is: INF ")

    # the example with multi path Loop sequence, 
    def while_multipath_ctldep(self):
        query = [1, 0, 1, 1, 0, 1]
        edges = [(0, 2), (0, 3), (0, 4), (1, 2), (1, 3),  (2, 2), (2, 5), (3, 3), (3, 5), (4, 4)]
        weights = [AdaptType("INF")]*len(query)

        ctl_edges = [(3, 4), (3, 5), (4, 6), (5, 6), (2, 3), (6, 2), (2, 7), (1, 2), (0, 1), (7, 8)]
        transitions = [
            (3, [], 4, []),
            (3, [], 5, []),
            (4, [ DifferenceConstraint("y", None, "Q", DifferenceConstraint.DCType.RESET) ], 6, [2]),
            (5, [ DifferenceConstraint("y", None, "Q", DifferenceConstraint.DCType.RESET) ], 6, [3]),
            (2, [], 3, []),
            (6, [ DifferenceConstraint("x", None, " 1 ", DifferenceConstraint.DCType.DEC) ], 2, [4]),
            (2, [], 7, []),
            (1, [ DifferenceConstraint("y", None, " 0 ", DifferenceConstraint.DCType.RESET) ], 2, [1]),
            (0, [ DifferenceConstraint("x", None, "Q", DifferenceConstraint.DCType.RESET) ], 1, [0]),
            (7, [ DifferenceConstraint("y", None, "Q", DifferenceConstraint.DCType.RESET) ], -1, [5])
        ]


        AdaptEstimate.adapt_estimate(Graph(edges, weights, query), TransitionGraph(ctl_edges, transitions))
        print("The Reachability Bounds Expected for  Vertices in while with If Banch inside are: [1, 1, Q, Q, Q, 1] ")

        print("The Adaptivity Expected for while with If Banch (Multi-Path While Loop) Algorithm is: 2 + Q")
        # print("The Adaptivity From This Graph is: ", adapt_search.get_adapt())


    # the nested while example, 
    # Expected Weights: [1, 1, k, k, k, k^2, k^2]
    def nested_while_valdep(self):
        # weights = [AdaptType(1), AdaptType(1), AdaptType("k"), AdaptType("k"), AdaptType("k"), AdaptType("k * k"), AdaptType("k * k")]
        query = [0, 1, 0, 0, 1, 0, 1]
        edges = [(0, 2), (1, 4), (1, 6), (2, 2), (2, 3), (2, 4), (3, 5), (3, 6), (5, 5), (5, 6), (6, 4), (6, 6)]
        weights = [AdaptType("INF")]*len(query)

        ctl_edges = [(7, 8), (6, 7), (8, 6), (5, 6), (4, 5), (3, 4), (2, 3), (6, 2), (1, 2), (0, 1), (2, 9)]
        transitions = [
                (7, [ DifferenceConstraint("j", None, " 1 ", DifferenceConstraint.DCType.DEC) ], 8, [6]),
                (6, [], 7, []),
                (8, [ DifferenceConstraint("x", None, "Q", DifferenceConstraint.DCType.RESET) ], 6, [5]),
                (5, [ DifferenceConstraint("y", None, "Q", DifferenceConstraint.DCType.RESET) ], 6, [4]),
                (4, [ DifferenceConstraint("j", None," k ", DifferenceConstraint.DCType.RESET) ], 5, [3]),
                (3, [ DifferenceConstraint("i", None, " 1 ", DifferenceConstraint.DCType.DEC) ], 4, [2]),
                (2, [], 3, []),
                (6, [], 2, []),
                (1, [ DifferenceConstraint("x", None, "Q", DifferenceConstraint.DCType.RESET) ], 2, [1]),
                (0, [ DifferenceConstraint("i", None," k ", DifferenceConstraint.DCType.RESET) ], 1, [0]),
                (2, [], -1, [])
            ] 

        AdaptEstimate.adapt_estimate(Graph(edges, weights, query), TransitionGraph(ctl_edges, transitions))

        # bound_infer = self.ALG(Graph(edges, weights, query), TransitionGraph(ctl_edges, transitions))
        # bound_infer.attach_weights()
        print("The Reachability Bounds Expected for  Vertices in the Simple Nested While Graph are: [1, 1, k, k, k, k^2, k^2] ")
        print("The Adaptivity Expected for Simple Nested While Algorithm is: 2 + k * k ")
        # print("The Adaptivity From This Graph is: ", adapt_search.get_adapt())

    # the nested while example, 
    # Expected Weights: [1, 1, k, k, k, k^2, k^2]
    def nested_while_recursivevaldep(self):
        # weights = [AdaptType(1), AdaptType(1), AdaptType("k"), AdaptType("k"), AdaptType("k"), AdaptType("k * k"), AdaptType("k * k")]
        query = [0, 1, 0, 0, 0, 1, 1]
        edges = [(1, 5), (5, 6), (6, 5)]
        weights = [AdaptType("INF")]*len(query)

        ctl_edges = [(6, 7), (5, 6), (7, 5), (5, 8), (4, 5), (3, 4), (2, 3), (8, 2), (1, 2), (0, 1), (2,  9)]
        transitions = [
               (6, [ DifferenceConstraint("j", None, " 1 ", DifferenceConstraint.DCType.DEC) ], 7, [4]),
                (5, [], 6, []),
                (7, [ DifferenceConstraint("y", None, "Q", DifferenceConstraint.DCType.RESET) ], 5, [5]),
                (5, [], 8, []),
                (4, [ DifferenceConstraint("j", None," k ", DifferenceConstraint.DCType.RESET) ], 5, [3]),
                (3, [ DifferenceConstraint("i", None, " 1 ", DifferenceConstraint.DCType.DEC) ], 4, [2]),
                (2, [], 3, []),
                (8, [ DifferenceConstraint("x", None, "Q", DifferenceConstraint.DCType.RESET) ], 2, [6]),
                (1, [ DifferenceConstraint("x", None, "Q", DifferenceConstraint.DCType.RESET) ], 2, [1]),
                (0, [ DifferenceConstraint("i", None," k ", DifferenceConstraint.DCType.RESET) ], 1, [0]),
                (2, [], -1, [])
            ] 

        AdaptEstimate.adapt_estimate(Graph(edges, weights, query), TransitionGraph(ctl_edges, transitions))

        # bound_infer = self.ALG(Graph(edges, weights, query), TransitionGraph(ctl_edges, transitions))
        # bound_infer.attach_weights()
        print("The Reachability Bounds Expected for  Vertices in the Simple Nested While Graph are: [1, 1, k, k, k^2, k^2, k^2] ")
        print("The Adaptivity Expected for Simple Nested While Algorithm is: 1 + 2 * k * k ")
        # print("The Adaptivity From This Graph is: ", adapt_search.get_adapt())


    # the nested while example, 
    # Expected Weights: [1, 1, k, k, k, k^2, k^2]
    def nested_while_multivaldep(self):
        # weights = [AdaptType(1), AdaptType(1), AdaptType("k"), AdaptType("k"), AdaptType("k"), AdaptType("k * k"), AdaptType("k * k")]
        query = [0, 1, 1, 0, 1, 0, 0, 1]
        edges = [(0, 3), (1, 4), (1, 7), (2, 4), (4, 4), (4, 7), (7, 4), (7, 7)]
        weights = [AdaptType("INF")]*len(query)

        ctl_edges = [(8, 9), (7, 8), (9, 7), (6, 7), (5, 6), (4, 5), (3, 4), (7, 3), (2, 3), (1, 2), (0, 1), (3, 10)]
        transitions = [
                (7, [ DifferenceConstraint("j", None, " 1 ", DifferenceConstraint.DCType.DEC) ], 8, [6]),
                (6, [], 7, []),
                (9, [ DifferenceConstraint("x", None, "Q", DifferenceConstraint.DCType.RESET) ], 7, [7]),
                (6, [ DifferenceConstraint("j", None, "k", DifferenceConstraint.DCType.RESET) ], 7, [5]),
                (5, [ DifferenceConstraint("z", None, "Q", DifferenceConstraint.DCType.RESET) ], 6, [4]),
                (4, [ DifferenceConstraint("i", None, " 1 ", DifferenceConstraint.DCType.DEC) ], 5, [3]),
                (2, [], 3, []),
                (6, [], 2, []),
                (2, [ DifferenceConstraint("z", None, "Q", DifferenceConstraint.DCType.RESET) ], 3, [2]),
                (1, [ DifferenceConstraint("x", None, "Q", DifferenceConstraint.DCType.RESET) ], 2, [1]),
                (0, [ DifferenceConstraint("i", None, "k", DifferenceConstraint.DCType.RESET) ], 1, [0]),
                (2, [], -1, [])
            ] 

        AdaptEstimate.adapt_estimate(Graph(edges, weights, query), TransitionGraph(ctl_edges, transitions))

        # bound_infer = self.ALG(Graph(edges, weights, query), TransitionGraph(ctl_edges, transitions))
        # bound_infer.attach_weights()
        print("The Reachability Bounds Expected for  Vertices in the Simple Nested While Graph are: [1, 1, k, k, k, k^2, k^2] ")
        print("The Adaptivity Expected for Simple Nested While Algorithm is: 1 + k + k * k ")
        # print("The Adaptivity From This Graph is: ", adapt_search.get_adapt())


    # the two-round example, 
    # Expected Weights: [1, 1, k, k, k, 1]
    # i.e., [AdaptType(1), AdaptType(1), AdaptType("k"), AdaptType("k"), AdaptType("k"), AdaptType(1)]
    def two_round(self):
        query = [0, 0, 0, 1, 0, 1]
        edges = [(0, 2), (1,4), (2,2), (2, 3), (2, 4), (3, 4), (4, 4), (4, 5)]
        weights = [AdaptType("INF")]*len(query)

        ctl_edges = [(4, 5), (3, 4), (2, 3), (5, 2), (2, 6), (1, 2), (0, 1), (6, 7)]
        transitions = [
            (4, [ DifferenceConstraint("a", None, "Q", DifferenceConstraint.DCType.RESET) ], 5, [3]),
            (3, [ DifferenceConstraint("i", None, " 1 ", DifferenceConstraint.DCType.DEC) ], 4, [2]),
            (2, [], 3, []),
            (5, [ DifferenceConstraint("l", None, "INF", DifferenceConstraint.DCType.RESET) ], 2, [4]),
            (2, [], 6, []),
            (1, [ DifferenceConstraint("l", None, " 0 ", DifferenceConstraint.DCType.RESET) ], 2, [1]),
            (0, [ DifferenceConstraint("i", None, " k ", DifferenceConstraint.DCType.RESET) ], 1, [0]),
            (6, [ DifferenceConstraint("y", None, "Q", DifferenceConstraint.DCType.RESET) ],7, [5])
            ] 

        AdaptEstimate.adapt_estimate(Graph(edges, weights, query), TransitionGraph(ctl_edges, transitions))
        print("The Reachability Bounds Expected for  Vertices in the Two Round Graph are: [1, 1, k, k, k, 1] ")
        # print("The Reachability Bounds Calculated for Vertices in This Graph are: ", bound_infer.get_weights())
       
        # adapt_search = AdaptSearchAlgRefined(bound_infer.graph)
        # adapt_search.search_adapt()
        print("The Adaptivity Expected for Two Round Algorithm is: 2 ")
        # print("The Adaptivity From This Graph is: ", adapt_search.get_adapt())


    # the multiple-round example, 
    # Expected Weights: [1, 1, k, k, k]
    # i.e., weights = [AdaptType(1),  AdaptType(1), AdaptType("k"), AdaptType("k"), AdaptType("k")]
    def multiple_round(self):
        query = [0, 0, 1, 0, 0]
        edges = [(0, 2), (0, 3), (0, 4), (1, 2), (1, 3), (2, 3), (3, 2), (3, 3), (4, 4)]
        weights = [AdaptType("INF")]*len(query)

        ctl_edges = [(4, 5), (3, 4), (2, 3), (5, 2), (1, 2), (0, 1), (2, 7)]

        transitions = [
            (4, [ DifferenceConstraint("l", None, "INF", DifferenceConstraint.DCType.RESET) ], 5, [3]),
            (3, [ DifferenceConstraint("a", None, "Q", DifferenceConstraint.DCType.RESET) ], 4, [2]),
            (2, [], 3, []),
            (5, [ DifferenceConstraint("i", None, " 1 ", DifferenceConstraint.DCType.DEC) ], 2, [4]),
            (1, [ DifferenceConstraint("l", None, " 0 ", DifferenceConstraint.DCType.RESET) ], 2, [1]),
            (0, [ DifferenceConstraint("i", None, " k ", DifferenceConstraint.DCType.RESET) ], 1, [0]),
            (2, [], -1, [])] 
        AdaptEstimate.adapt_estimate(Graph(edges, weights, query), TransitionGraph(ctl_edges, transitions))

        # bound_infer = self.ALG(Graph(edges, weights, query), TransitionGraph(ctl_edges, transitions))
        # bound_infer.attach_weights()
        print("The Reachability Bounds Expected for  Vertices in the Multiple Round Graph are: [1, 1, k, k, k] ")
        # print("The Reachability Bounds Calculated for Vertices in This Graph are: ", bound_infer.get_weights())

        # adapt_search = AdaptSearchAlgRefined(bound_infer.graph)
        # adapt_search.search_adapt()
        print("The Adaptivity Expected for multiple Round Algorithm is: k ")
        # print("The Adaptivity From This Graph is: ", adapt_search.get_adapt())



    
    def run_tests(self):
        self.seq()





test_bound_infer = TestUnits(VariableReachingBound)
test_bound_infer.seq()
test_bound_infer.seq_multivar()
test_bound_infer.if_valdep()
test_bound_infer.if_ctldep()
test_bound_infer.two_round()
test_bound_infer.multiple_round()
test_bound_infer.while_valdep()
test_bound_infer.nested_while_valdep()
test_bound_infer.while_multipath()
test_bound_infer.while_multivaldep()
test_bound_infer.while_valctldep()
test_bound_infer.while_multipath_ctldep()
test_bound_infer.nested_while_multivaldep()
test_bound_infer.nested_while_recursivevaldep()
