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
        transition_graph = GraphParser("./examples/ps_reachability_bound/seq.br").abscfg_parse()
        self.runner(transition_graph, refined_prog)


    # the example with while loop of multi-path from if branch (multi-path loop will result in different visiting times for
    # verteices  belong to the same loop),  
    # Expected Path Sensitive Reachability Bounds: 1, 
    # Ouput Reachability Bounds: 
    def multiple_round_odd_sim(self):
        refined_prog = RefinedProg(RefinedProg.RType.REPEAT, 
            RefinedProg(RefinedProg.RType.CHOICE, 
                [RefinedProg(RefinedProg.RType.SEQ, 
                    [RefinedProg(RefinedProg.RType.REPEAT, RefinedProg(RefinedProg.RType.TP, [2, 0, 3])),
                    RefinedProg(RefinedProg.RType.TP, [2, 1, 4])]),
                 RefinedProg(RefinedProg.RType.SEQ, 
                    [RefinedProg(RefinedProg.RType.REPEAT, RefinedProg(RefinedProg.RType.TP, [2, 1, 4])),
                    RefinedProg(RefinedProg.RType.TP, [2, 0, 3])])
                    ]))
        print("The Reachability Bounds Expected for  Vertices in the Multiple Path Odd While Graph are: [1, 1, k, k/2, k/2, k] ")
        print("The Reachability Bounds Expected for  Vertices in the Multiple Path Odd While Graph are: [1, 1, k, k/2, k/2, k] ")

        transition_graph = GraphParser("./examples/ps_reachability_bound/multiple_round_odd_sim.br").abscfg_parse()
        self.runner(transition_graph, refined_prog)

    # the example with while loop of multi-path from if branch (multi-path loop will result in different visiting times for
    # verteices  belong to the same loop),  
    # Expected Path Sensitive Reachability Bounds: 1, 
    # Ouput Reachability Bounds: 
    def multiple_round_single_sim(self):
        refined_prog = RefinedProg(RefinedProg.RType.CHOICE, 
                [RefinedProg(RefinedProg.RType.REPEAT, RefinedProg(RefinedProg.RType.TP, [2, 0, 3])),
                RefinedProg(RefinedProg.RType.REPEAT, RefinedProg(RefinedProg.RType.TP, [2, 1, 4])),
                RefinedProg(RefinedProg.RType.REPEAT, 
                    RefinedProg(RefinedProg.RType.SEQ,
                        [RefinedProg(RefinedProg.RType.REPEAT, RefinedProg(RefinedProg.RType.TP, [2, 0, 3])),
                        RefinedProg(RefinedProg.RType.TP, [2, 1, 4])]))])

        print("The Reachability Bounds Expected for  Vertices in the Multiple Path Single While Graph are: [1, 1, k, k/2, k/2, k] ")
        print("The Reachability Bounds Expected for  Vertices in the Multiple Path Single While Graph are: [1, 1, k, k/2, k/2, k] ")

        transition_graph = GraphParser("./examples/ps_reachability_bound/multiple_round_single_sim.br").abscfg_parse()
        self.runner(transition_graph, refined_prog)

    # the example with while loop of multi-path from if branch (multi-path loop will result in different visiting times for
    # verteices  belong to the same loop),  
    # Expected Path Sensitive Reachability Bounds: 1, 
    # Ouput Reachability Bounds: 
    def while_two_counters(self):
        refined_prog = RefinedProg(RefinedProg.RType.CHOICE, 
                [RefinedProg(RefinedProg.RType.REPEAT, RefinedProg(RefinedProg.RType.TP, [3, 0, 2, 4])),
                RefinedProg(RefinedProg.RType.REPEAT, 
                    RefinedProg(RefinedProg.RType.SEQ,
                        [RefinedProg(RefinedProg.RType.REPEAT, RefinedProg(RefinedProg.RType.TP, [3, 0, 2, 4])),
                        RefinedProg(RefinedProg.RType.TP, [3, 1, 5])]))])

        print("The Reachability Bounds Expected for  Vertices in the Multiple Path Single While Graph are: [1, 1, k, k/2, k/2, k] ")
        print("The Reachability Bounds Expected for  Vertices in the Multiple Path Single While Graph are: [1, 1, k, k/2, k/2, k] ")

        transition_graph = GraphParser("./examples/ps_reachability_bound/while_two_counters.br").abscfg_parse()
        self.runner(transition_graph, refined_prog)

    def three_nested_while(self):
        refined_prog = RefinedProg(RefinedProg.RType.REPEAT, 
        RefinedProg(RefinedProg.RType.SEQ,
                [RefinedProg(RefinedProg.RType.TP, [1, 3]),
                RefinedProg(RefinedProg.RType.REPEAT, 
                    RefinedProg(RefinedProg.RType.SEQ,
                        [ RefinedProg(RefinedProg.RType.TP, [4, 6, 7]),
                        RefinedProg(RefinedProg.RType.REPEAT, 
                        RefinedProg(RefinedProg.RType.TP, [8, 10])),
                        RefinedProg(RefinedProg.RType.TP, [9, 11])])),
                        RefinedProg(RefinedProg.RType.TP, [12, 2])])
                        )

        print("The Reachability Bounds Expected for  Vertices in the Three Level Nested While Graph are: [1, 1, k, k/2, k/2, k] ")
        print("The Reachability Bounds Expected for  Vertices in the Three Level Nested While Graph are: [1, 1, k, k/2, k/2, k] ")

        transition_graph = GraphParser("./examples/ps_reachability_bound/nested_while_three.br").abscfg_parse()
        self.runner(transition_graph, refined_prog)
        
    
    def run_tests(self):
        self.test_seq()
        self.multiple_round_single_sim()
        self.while_two_counters()
        self.three_nested_while()



tester = TestUnits(PathSensitiveReachabilityBound)
tester.run_tests()

