from rechability_bound_pathsensitive import PathSensitiveReachabilityBound, RefinedProg
from bound_infer import DifferenceConstraint, TransitionGraph, TransitionBound
from adapt_base import AdaptType
from graph_parse import GraphParser

class TestUnits:

    def __init__(self, ALG) -> None:
        self.ALG = ALG

    def runner(self, transition_graph, refined_prog):
        pathsensitive_rb = self.ALG(transition_graph)
        pathsensitive_rb.compute_rb(refined_prog)
        bound_infer = TransitionBound(transition_graph)
        bound_infer.compute_transition_bounds()
        
        print("The Reachability Bounds Calculated for Transitions in This Graph are: ")
        bound_infer.print_transition_bounds()
        print("The Calculated Path Sensitive Reachability Bounds are: ") 
        pathsensitive_rb.print_path_bound()

    # test the default graph
    def test_default(self):
        bound_infer = self.ALG()
        bound_infer.compute_transition_bounds()
        print("The Reachability Bounds Calculated for Vertices in Default Graph are: ", bound_infer.print_transition_bounds())

    # the example with only sequence, 
    # Expected Weights: [1,1,1,1]
    def test_seq(self):
        ctl_edges = [(2, 3), (1, 2), (0, 1), (3, 4)]
        transitions = [
            (2, [ DifferenceConstraint("z", None, "Q", DifferenceConstraint.DCType.RESET) ], 3, [2]),
            (1, [ DifferenceConstraint("y", None, "Q", DifferenceConstraint.DCType.RESET) ], 2, [1]),
            (0, [ DifferenceConstraint("x", None, "Q", DifferenceConstraint.DCType.RESET) ], 1, [0]),
            (3, [ DifferenceConstraint("w", None, "Q", DifferenceConstraint.DCType.RESET) ], -1, [3])
            ]
        refined_prog = RefinedProg(RefinedProg.RType.TP, [0, 1, 2, 3])
        print("The Reachability Bounds Expected for Vertices in Simple Sequence Graph are: [1,1,1,1] ")
        print("The Path Sensitive Reachability Bounds Expected for All Transitions in Simple Sequence Graph are: [1,1,1,1] ")
        transition_graph = GraphParser("./examples/seq.br").abscfg_parse()
        self.runner(transition_graph, refined_prog)


    # the example with while loop of multi-path from if branch (multi-path loop will result in different visiting times for
    # verteices  belong to the same loop),  
    # Expected Adaptivity: 1 + k
    # Ouput Adaptivity: 1 + k/2 + k
    def multiple_round_odd_sim(self):
        ctl_edges = [(0, 1), (1, 2), (2, 3), (2, 3), (3, 1)]
        transitions = [
            (0, [DifferenceConstraint("i", None, "k", DifferenceConstraint.DCType.RESET),
            DifferenceConstraint("x", None, "Q", DifferenceConstraint.DCType.RESET)], 1, [0, 1]),
            (1, [DifferenceConstraint("i", None, "1", DifferenceConstraint.DCType.DEC)], 2, [2]),
            (2, [DifferenceConstraint("y", None, "Q", DifferenceConstraint.DCType.RESET)], 3, [3]),
            (2, [DifferenceConstraint("y", None, "Q", DifferenceConstraint.DCType.RESET)], 3, [4]),
            (3, [DifferenceConstraint("x", None, "Q", DifferenceConstraint.DCType.RESET)], 2, [5])
            ]
        refined_prog = RefinedProg(RefinedProg.RType.REPEAT, 
            RefinedProg(RefinedProg.RType.CHOICE, 
                [RefinedProg(RefinedProg.RType.SEQ, 
                    [RefinedProg(RefinedProg.RType.REPEAT, RefinedProg(RefinedProg.RType.TP, [2, 0, 3])),
                    RefinedProg(RefinedProg.RType.TP, [2, 1, 4])]),
                 RefinedProg(RefinedProg.RType.SEQ, 
                    [RefinedProg(RefinedProg.RType.REPEAT, RefinedProg(RefinedProg.RType.TP, [2, 1, 4])),
                    RefinedProg(RefinedProg.RType.TP, [2, 0, 3])])
                    ]))
        print("The Reachability Bounds Expected for  Vertices in the Multiple Path Simple While Graph are: [1, 1, k, k/2, k/2, k] ")
        print("The Reachability Bounds Expected for  Vertices in the Multiple Path Simple While Graph are: [1, 1, k, k/2, k/2, k] ")

        transition_graph = GraphParser("./examples/multiple_round_odd_sim.br").abscfg_parse()
        self.runner(transition_graph, refined_prog)


    # the example with simple Loop sequence, 
    # Expected Weights: [1,k,k,k]
    def test_simple_while(self):
        ctl_edges = [(0, 1), (1, 1)]
        transitions = [(0, [DifferenceConstraint("x", None, "k", DifferenceConstraint.DCType.RESET)], 1, [0]),
        (1, [DifferenceConstraint("x", None, "1", DifferenceConstraint.DCType.DEC)], 1, [1, 2, 3])]

        bound_infer = self.ALG(TransitionGraph(ctl_edges, transitions))
        bound_infer.compute_transition_bounds()
        print("The Reachability Bounds Calculated for Vertices in This Graph are: ", bound_infer.print_transition_bounds())
        print("The Reachability Bounds Expected for  Vertices in Simple while are: [1, k, k,k] ")
        
    # the two-round example, 
    # Expected Weights: [1,k,k,1]
    # i.e., [AdaptType(1), AdaptType("k"), AdaptType("k"), AdaptType(1)]
    def test_multiple_constriants(self):
        ctl_edges = [(0, 1), (1, 1), (1, 2)]
        transitions = [(0, 
        [DifferenceConstraint("i", None, "k", DifferenceConstraint.DCType.RESET),
        DifferenceConstraint("l", None, "Q", DifferenceConstraint.DCType.RESET)
        ], 1, [0, 1]),
        (1, [DifferenceConstraint("i", None, "1", DifferenceConstraint.DCType.DEC),
        DifferenceConstraint("a", None, "Q", DifferenceConstraint.DCType.RESET),
        DifferenceConstraint("l", "a", "0", DifferenceConstraint.DCType.INC)], 1, [2, 3, 4]),
        (1, [DifferenceConstraint("b", "l", "Q", DifferenceConstraint.DCType.RESET)], 2, [5])]


        bound_infer = self.ALG(TransitionGraph(ctl_edges, transitions))
        bound_infer.compute_transition_bounds()
        print("The Reachability Bounds Calculated for Vertices in This Graph are: ", bound_infer.print_transition_bounds())
        print("The Reachability Bounds Expected for  Vertices in the Two Round Graph are: [1, 1, k, k, k, 1] ")
       



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


        bound_infer = self.ALG(TransitionGraph(ctl_edges, transitions))
        bound_infer.compute_transition_bounds()
        print("The Reachability Bounds Calculated for Vertices in This Graph are: ", bound_infer.print_transition_bounds())
        print("The Reachability Bounds Expected for  Vertices in the Simple Nested While Graph are: [1, k, k, k!] ")

        # adapt_search = AdaptSearchAlgRefined(bound_infer.graph)
        # adapt_search.search_adapt()
        # print("The Adaptivity Expected for Simple Nested While Algorithm is: 2 + k! ")
        # print("The Adaptivity From This Graph is: ", adapt_search.get_adapt())

    # the example in if branch and only value dependency, 
    # Expected Adaptivity: 2
    # Ouput Adaptivity: 2
    def test_if_val(self):
        ctl_edges = [(0, 1), (1, 2), (2, 3), (2, 3), (3, 1)]
        transitions = [
            (0, [DifferenceConstraint("i", None, "k", DifferenceConstraint.DCType.RESET),
            DifferenceConstraint("x", None, "Q", DifferenceConstraint.DCType.RESET)], 1, [0, 1]),
            (1, [DifferenceConstraint("i", None, "1", DifferenceConstraint.DCType.DEC)], 2, [2]),
            (2, [DifferenceConstraint("y", None, "Q", DifferenceConstraint.DCType.RESET)], 3, [3]),
            (2, [DifferenceConstraint("y", None, "Q", DifferenceConstraint.DCType.RESET)], 3, [4]),
            (3, [DifferenceConstraint("x", None, "Q", DifferenceConstraint.DCType.RESET)], 2, [5])
            ]
        
        bound_infer = self.ALG(TransitionGraph(ctl_edges, transitions))
        bound_infer.compute_transition_bounds()
        print("The Reachability Bounds Calculated for Vertices in This Graph are: ", bound_infer.print_transition_bounds())

        print("The Adaptivity Expected for This Graph is: 2 ")



    # the example with while loop of multi-path from if branch (multi-path loop will result in different visiting times for
    # verteices  belong to the same loop),  
    # Expected Adaptivity: 1 + k
    # Ouput Adaptivity: 1 + k/2 + k
    def test_while_multipath_if(self):
        ctl_edges = [(0, 1), (1, 2), (2, 3), (2, 3), (3, 1)]
        transitions = [
            (0, [DifferenceConstraint("i", None, "k", DifferenceConstraint.DCType.RESET),
            DifferenceConstraint("x", None, "Q", DifferenceConstraint.DCType.RESET)], 1, [0, 1]),
            (1, [DifferenceConstraint("i", None, "1", DifferenceConstraint.DCType.DEC)], 2, [2]),
            (2, [DifferenceConstraint("y", None, "Q", DifferenceConstraint.DCType.RESET)], 3, [3]),
            (2, [DifferenceConstraint("y", None, "Q", DifferenceConstraint.DCType.RESET)], 3, [4]),
            (3, [DifferenceConstraint("x", None, "Q", DifferenceConstraint.DCType.RESET)], 2, [5])
            ]


        bound_infer = self.ALG(TransitionGraph(ctl_edges, transitions))
        bound_infer.compute_transition_bounds()
        print("The Reachability Bounds Calculated for Vertices in This Graph are: ", bound_infer.print_transition_bounds())
        print("The Reachability Bounds Expected for  Vertices in the Multiple Path Simple While Graph are: [1, 1, k, k/2, k/2, k] ")
        refined_prog = RefinedProg(RefinedProg.RType.REPEAT, 
            [RefinedProg(RefinedProg.RType.CHOICE, 
                [RefinedProg(RefinedProg.RType.SEQ, 
                    [RefinedProg(RefinedProg.RType.REPEAT, RefinedProg(RefinedProg.RType.TP, [2, 0, 3])),
                    RefinedProg(RefinedProg.RType.TP, [2, 1, 4])]),
                 RefinedProg(RefinedProg.RType.SEQ, 
                    [RefinedProg(RefinedProg.RType.REPEAT, RefinedProg(RefinedProg.RType.TP, [2, 1, 4])),
                    RefinedProg(RefinedProg.RType.TP, [2, 0, 3])])
                    ])])
        pathsensitive_rb = self.ALG(TransitionGraph(ctl_edges, transitions))
        pathsensitive_rb.compute_rb(refined_prog)
        print("The Reachability Bounds Calculated for Vertices in This Program are: ", pathsensitive_rb.print_path_bound())
        print("The Reachability Bounds Expected for  Vertices in the Multiple Path Simple While Graph are: [1, 1, k, k/2, k/2, k] ")




    # the two-round example, 
    # Expected Weights: [1,k,k,1]
    # i.e., [AdaptType(1), AdaptType("k"), AdaptType("k"), AdaptType(1)]
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

        bound_infer = self.ALG(TransitionGraph(ctl_edges, transitions))
        bound_infer.compute_transition_bounds()
        print("The Reachability Bounds Calculated for Vertices in This Graph are: ", bound_infer.print_transition_bounds())
        print("The Reachability Bounds Expected for  Vertices in the Two Round Graph are: [1, 1, k, k, k, 1] ")
       

    # the multiple-round example, 
    # Expected Weights: [1, 1, k, k, k]
    # i.e., weights = [AdaptType(1),  AdaptType(1), AdaptType("k"), AdaptType("k"), AdaptType("k")]
    def test_multiple_round(self):
        ctl_edges = [(0, 1), (1, 2), (2, 3), (3, 4), (4, 5), (5, 2)]

        transitions = [
            (0, [DifferenceConstraint("i", None, "k", DifferenceConstraint.DCType.RESET)], 1, [0]),
            (1, [DifferenceConstraint("I", None, "[]", DifferenceConstraint.DCType.RESET)], 2, [1]),
            (1, [], 1, []),
            (1, [DifferenceConstraint("i", None, "1", DifferenceConstraint.DCType.DEC)], 1, [2]),
            (1, [DifferenceConstraint("a", None, "Q", DifferenceConstraint.DCType.RESET)], 1, [3]),
            (1, [DifferenceConstraint("I", "a", "0", DifferenceConstraint.DCType.INC)], 1, [4])] 
        bound_infer = self.ALG(TransitionGraph(ctl_edges, transitions))
        bound_infer.compute_transition_bounds()
        print("The Reachability Bounds Calculated for Vertices in This Graph are: ", bound_infer.print_transition_bounds())
        print("The Reachability Bounds Expected for  Vertices in the Multiple Round Graph are: [1, 1, k, k, k] ")


    # the nested while example, 
    # Expected Weights: [1, 1, k, k, k, k^2, k^2]
    def test_nested_while(self):
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


        bound_infer = self.ALG(TransitionGraph(ctl_edges, transitions))
        bound_infer.compute_transition_bounds()
        print("The Reachability Bounds Calculated for Vertices in This Graph are: ", bound_infer.print_transition_bounds())
        print("The Reachability Bounds Expected for  Vertices in the Simple Nested While Graph are: [1, 1, k, k, k, k^2, k^2] ")

    
    def run_tests(self):
        self.test_seq()
        self.test_if_val()
        self.test_two_round()
        self.test_while_multipath_if()
        self.test_default()
        self.test_nested_while()
        self.test_simple_while()
        self.test_simple_nested_approximated()
        self.test_multiple_round()
        self.test_multiple_constriants()




tester = TestUnits(PathSensitiveReachabilityBound)
tester.test_seq()
tester.multiple_round_odd_sim()

