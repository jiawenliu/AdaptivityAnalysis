from bound_infer import TransitionGraph, TransitionBound, DifferenceConstraint, VariableReachingBound
from adapt_search_refined import Graph, AdaptType, AdaptSearchAlgRefined

class TestUnits:

    def __init__(self, ALG) -> None:
        self.ALG = ALG

    # the example with only sequence, 
    # Expected Weights: [1,1,1,1]
    def test_seq(self):
        weights = [AdaptType(0), AdaptType(0), AdaptType(0), AdaptType(0)]
        query = [1, 1, 1, 1]
        edges = [(0, 1), (1, 2), (1, 3), (2, 3)]
        # adapt_search = self.ALG(Graph(edges, weights, query))

        ctl_edges = [(0, 1)]
        transitions = [(0, [DifferenceConstraint("x", None, "k", DifferenceConstraint.DCType.RESET)], 1, [0, 1, 2 ,3])]


        bound_infer = self.ALG(Graph(edges, weights, query), TransitionGraph(ctl_edges, transitions))
        bound_infer.attach_weights()
        print("The Reachability Bounds Expected for  Vertices in Pure Sequence Graph are: [1,1,1,1] ")
        print("The Reachability Bounds Calculated for Vertices in This Graph are: ", bound_infer.get_weights())

        adapt_search = AdaptSearchAlgRefined(bound_infer.graph)
        adapt_search.search_adapt()
        print("The Adaptivity Expected for Simple Seq Algorithm is: 4 ")
        print("The Adaptivity From This Graph is: ", adapt_search.get_adapt())

    # the example in if branch and only value dependency, 
    # Expected Adaptivity: 2
    # Ouput Adaptivity: 2
    def test_if_valdep(self):
        query = [1, 0, 1, 1]
        edges = [(0, 2), (1, 3), (1, 2)]
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


        bound_infer = self.ALG(Graph(edges, weights, query), TransitionGraph(ctl_edges, transitions))
        bound_infer.attach_weights()
        print("The Reachability Bounds Expected for Mutli-path Dependency in If branch Algorithm are: [1,1,1,1] ")
        print("The Reachability Bounds Calculated for Vertices in This Graph are: ", bound_infer.get_weights())

        adapt_search = AdaptSearchAlgRefined(bound_infer.graph)
        adapt_search.search_adapt()
        print("The Adaptivity Expected for Mutli-path Dependency in If branch Algorithm is: 2 ")
        print("The Adaptivity From This Graph is: ", adapt_search.get_adapt())


    def test_if_ctldep(self):
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


        bound_infer = self.ALG(Graph(edges, weights, query), TransitionGraph(ctl_edges, transitions))
        bound_infer.attach_weights()
        print("The Reachability Bounds Expected for  Vertices in If Branch with Control Dependency Graph are: [1, 1, 1, 1] ")
        print("The Reachability Bounds Calculated for Vertices in This Graph are: ", bound_infer.get_weights())

        adapt_search = AdaptSearchAlgRefined(bound_infer.graph)
        adapt_search.search_adapt()
        print("The Adaptivity Expected for Mutli-path Dependency in If branch Algorithm is: 3 ")
        print("The Adaptivity From This Graph is: ", adapt_search.get_adapt())

    # the example with simple Loop sequence, 
    # Expected Weights: [1,k,k,k]
    def test_simple_while(self):
        query = [0, 1, 1, 0]
        edges = [(0, 2), (1, 2), (1, 3)]
        weights = [AdaptType("INF")]*len(query)

        ctl_edges = [(0, 1), (1, 1)]
        transitions = [(0, [DifferenceConstraint("x", None, "k", DifferenceConstraint.DCType.RESET)], 1, [0]),
        (1, [DifferenceConstraint("x", None, "1", DifferenceConstraint.DCType.DEC)], 1, [1, 2, 3])]


        bound_infer = self.ALG(Graph(edges, weights, query), TransitionGraph(ctl_edges, transitions))
        bound_infer.attach_weights()
        print("The Reachability Bounds Expected for  Vertices in Simple while are: [1, k, k,k] ")
        print("The Reachability Bounds Calculated for Vertices in This Graph are: ", bound_infer.get_weights())
        
        adapt_search = AdaptSearchAlgRefined(bound_infer.graph)
        adapt_search.search_adapt()
        print("The Adaptivity Expected for Simple While Algorithm is: 1 ")
        print("The Adaptivity From This Graph is: ", adapt_search.get_adapt())

    # the example with multi path Loop sequence, 
    def test_multipath_while(self):
        query = [0, 1, 0, 1, 1, 1]
        edges = [(0, 2), (2, 3), (1, 3), (1, 4), (1, 5), (3, 5), (5, 3), (5, 4)]
        # edges = [(0, 4), (1, 2), (1, 3), (2, 3), (3, 2), (3, 3)]
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


        bound_infer = self.ALG(Graph(edges, weights, query), TransitionGraph(ctl_edges, transitions))
        bound_infer.attach_weights()
        print("The Reachability Bounds Expected for  Vertices in while with If Banch inside are: [1, 1, k, k/2, k/2, k] ")
        print("The Reachability Bounds Calculated for Vertices in This Graph are: ", bound_infer.get_weights())
        
        adapt_search = AdaptSearchAlgRefined(bound_infer.graph)
        adapt_search.search_adapt()
        print("The Adaptivity Expected for while with If Banch (Multi-Path While Loop) Algorithm is: 1 + k/2 + k/2")
        print("The Adaptivity From This Graph is: ", adapt_search.get_adapt())

    # the nested while example, 
    # Expected Weights: [1, 1, k, k, k, k^2, k^2]
    def test_nested_while_valdep(self):
        # weights = [AdaptType(1), AdaptType(1), AdaptType("k"), AdaptType("k"), AdaptType("k"), AdaptType("k * k"), AdaptType("k * k")]
        query = [0, 1, 0, 0, 1, 0, 1]
        edges = [(0, 2), (1, 4), (1, 6), (2, 2), (2, 3), (3, 5), (5, 5), (6, 4), (6, 6)]
        weights = [AdaptType("INF")]*len(query)

        ctl_edges = [(0, 1), (1, 2), (2, 3), (3, 4), (4, 5), (5, 6), (6, 7), (6, 2), (7, 8), (8, 6)]
        transitions = [
            (0, [DifferenceConstraint("i", None, "k", DifferenceConstraint.DCType.RESET)], 1, [0]),
            (1, [DifferenceConstraint("x", None, "Q", DifferenceConstraint.DCType.RESET)], 2, [1]),
            (1, [], 1, []),
            (1, [DifferenceConstraint("i", None, "1", DifferenceConstraint.DCType.DEC)], 1, [2]),
            (1, [DifferenceConstraint("j", None, "k", DifferenceConstraint.DCType.RESET)], 1, [3]),
            (1, [DifferenceConstraint("y", "x", "Q", DifferenceConstraint.DCType.RESET)], 1, [4]),
            (1, [], 1, []),
            (1, [], 1, []),
            (1, [DifferenceConstraint("j", None, "1", DifferenceConstraint.DCType.DEC)], 2, [5]),
            (1, [DifferenceConstraint("x", None, "Q", DifferenceConstraint.DCType.RESET)], 2, [6])
            ] 


        bound_infer = self.ALG(Graph(edges, weights, query), TransitionGraph(ctl_edges, transitions))
        bound_infer.attach_weights()
        print("The Reachability Bounds Expected for  Vertices in the Simple Nested While Graph are: [1, 1, k, k, k, k^2, k^2] ")
        print("The Reachability Bounds Calculated for Vertices in This Graph are: ", bound_infer.get_weights())

        adapt_search = AdaptSearchAlgRefined(bound_infer.graph)
        adapt_search.search_adapt()
        print("The Adaptivity Expected for Simple Nested While Algorithm is: 2 + k * k ")
        print("The Adaptivity From This Graph is: ", adapt_search.get_adapt())


    # the nested while example, 
    # Expected Weights: [1, 1, k, k, k!]
    def test_simple_nested_approximated(self):
        # weights = [AdaptType(1), AdaptType("k"), AdaptType("k"), AdaptType(1)]
        query = [0, 0, 0, 1, 0, 1]
        edges = [(0, 4), (1, 2), (1, 3), (2, 3), (3, 2), (3, 3)]
        weights = [AdaptType("INF")]*len(query)

        ctl_edges = [(0, 1), (1, 2), (2, 1), (2, 2)]
        transitions = [
            (0, [DifferenceConstraint("i", None, "k", DifferenceConstraint.DCType.RESET)], 1, [0]),
            (1, [DifferenceConstraint("i", None, "1", DifferenceConstraint.DCType.DEC),
            DifferenceConstraint("j", "i", "0", DifferenceConstraint.DCType.RESET)], 2, [1, 2]),
            (2, [DifferenceConstraint("j", None, "1", DifferenceConstraint.DCType.DEC)], 2, [3]),
            (2, [], 1, [])
            ]


        bound_infer = self.ALG(Graph(edges, weights, query), TransitionGraph(ctl_edges, transitions))
        bound_infer.attach_weights()
        print("The Reachability Bounds Expected for  Vertices in the Simple Nested While Graph are: [1, k, k, k!] ")
        print("The Reachability Bounds Calculated for Vertices in This Graph are: ", bound_infer.get_weights())

        adapt_search = AdaptSearchAlgRefined(bound_infer.graph)
        adapt_search.search_adapt()
        print("The Adaptivity Expected for Simple Nested While Algorithm is: 2 + k! ")
        print("The Adaptivity From This Graph is: ", adapt_search.get_adapt())


    # the two-round example, 
    # Expected Weights: [1, 1, k, k, k, 1]
    # i.e., [AdaptType(1), AdaptType(1), AdaptType("k"), AdaptType("k"), AdaptType("k"), AdaptType(1)]
    def test_two_round(self):
        query = [0, 0, 0, 1, 0, 1]
        edges = [(0, 2), (1, 2), (1,4), (2,2), (2, 3), (3, 4), (4, 4), (4, 5)]
        weights = [AdaptType("INF")]*len(query)

        ctl_edges = [(0, 1), (1, 2), (2, 3), (3, 4), (4, 5), (5, 2), (2, 6)]
        transitions = [
            (0, [DifferenceConstraint("i", None, "k", DifferenceConstraint.DCType.RESET)], 1, [0]),
            (1, [DifferenceConstraint("l", None, "Q", DifferenceConstraint.DCType.RESET)], 2, [1]),
            (1, [DifferenceConstraint("i", None, "1", DifferenceConstraint.DCType.DEC)], 1, [2]),
            (1, [DifferenceConstraint("a", None, "Q", DifferenceConstraint.DCType.RESET)], 1, [3]),
            (1, [DifferenceConstraint("l", "a", "0", DifferenceConstraint.DCType.INC)], 1, [4]),
            (1, [], 1, []),
            (1, [DifferenceConstraint("b", "l", "Q", DifferenceConstraint.DCType.RESET)], 2, [5])] 

        bound_infer = self.ALG(Graph(edges, weights, query), TransitionGraph(ctl_edges, transitions))
        bound_infer.attach_weights()
        print("The Reachability Bounds Expected for  Vertices in the Two Round Graph are: [1, 1, k, k, k, 1] ")
        print("The Reachability Bounds Calculated for Vertices in This Graph are: ", bound_infer.get_weights())
       
        adapt_search = AdaptSearchAlgRefined(bound_infer.graph)
        adapt_search.search_adapt()
        print("The Adaptivity Expected for Two Round Algorithm is: 2 ")
        print("The Adaptivity From This Graph is: ", adapt_search.get_adapt())


    # the multiple-round example, 
    # Expected Weights: [1, 1, k, k, k]
    # i.e., weights = [AdaptType(1),  AdaptType(1), AdaptType("k"), AdaptType("k"), AdaptType("k")]
    def test_multiple_round(self):
        query = [0, 0, 0, 1, 0]
        edges = [(0, 4), (1, 2), (1, 3), (2, 3), (3, 2), (3, 3), (4, 4)]
        weights = [AdaptType("INF")]*len(query)

        ctl_edges = [(0, 1), (1, 2), (2, 3), (3, 4), (4, 5), (5, 2)]

        transitions = [
            (0, [DifferenceConstraint("i", None, "k", DifferenceConstraint.DCType.RESET)], 1, [0]),
            (1, [DifferenceConstraint("I", None, "[]", DifferenceConstraint.DCType.RESET)], 2, [1]),
            (1, [], 1, []),
            (1, [DifferenceConstraint("i", None, "1", DifferenceConstraint.DCType.DEC)], 1, [2]),
            (1, [DifferenceConstraint("a", None, "Q", DifferenceConstraint.DCType.RESET)], 1, [3]),
            (1, [DifferenceConstraint("I", "a", "0", DifferenceConstraint.DCType.INC)], 1, [4])] 
        bound_infer = self.ALG(Graph(edges, weights, query), TransitionGraph(ctl_edges, transitions))
        bound_infer.attach_weights()
        print("The Reachability Bounds Expected for  Vertices in the Multiple Round Graph are: [1, 1, k, k, k] ")
        print("The Reachability Bounds Calculated for Vertices in This Graph are: ", bound_infer.get_weights())

        adapt_search = AdaptSearchAlgRefined(bound_infer.graph)
        adapt_search.search_adapt()
        print("The Adaptivity Expected for multiple Round Algorithm is: k ")
        print("The Adaptivity From This Graph is: ", adapt_search.get_adapt())



    
    def run_tests(self):
        self.test_seq()
        self.test_two_round()
        self.test_while_multipath_if()




test_bound_infer = TestUnits(VariableReachingBound)
test_bound_infer.test_multipath_while()
