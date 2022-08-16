from program_refine import ProgramRefine, RefinedProg
from graph_parse import GraphParser

class TestUnits:

    def __init__(self, ALG) -> None:
        self.ALG = ALG


    # the example with only sequence, 
    # Expected Weights: [1,1,1,1]
    def test_seq(self):
        #TODO: implment the refined_prog
        # refined_prog = GraphParser("./examples/ps_reachability_bound/seq.br").refined_prog_parse()
        transition_graph = GraphParser("./examples/ps_reachability_bound/seq.br").abscfg_parse()
        refined_prog = ProgramRefine(transition_graph).program_refine()
        expected_refined_prog = RefinedProg(RefinedProg.RType.TP, [0, 1, 2, 3])
        print("The Expected Refined Program for Simple Sequence Program is: ", expected_refined_prog.prog_id(), expected_refined_prog.prog_signature())
        print("The Computed Refined Program for Simple Sequence Program is: ", refined_prog.prog_id(), refined_prog.prog_signature())


    def while_sim(self):
        expected_refined_prog = RefinedProg(RefinedProg.RType.SEQ, 
        [RefinedProg(RefinedProg.RType.TP, [0, 1]),
        RefinedProg(RefinedProg.RType.REPEAT, RefinedProg(RefinedProg.RType.TP, [2, 4, 5, 6])),
        RefinedProg(RefinedProg.RType.TP, [3, 7])
        ])
        transition_graph = GraphParser("./examples/ps_reachability_bound/while_sim.br").abscfg_parse()
        refined_prog = ProgramRefine(transition_graph).program_refine()
        print("The Expected Refined Program for Simple While Program is: ", expected_refined_prog.prog_id(), expected_refined_prog.prog_signature())
        print("The Computed Refined Program for Simple While Program is: ", refined_prog.prog_id(),refined_prog.prog_signature())

    # the example with while loop of multi-path from if branch (multi-path loop will result in different visiting times for
    # verteices  belong to the same loop),  
    # Expected Path Sensitive Reachability Bounds: 1, 
    # Ouput Reachability Bounds: 
    def multiple_round_odd_sim(self):
        expected_refined_prog = RefinedProg(RefinedProg.RType.REPEAT, 
            RefinedProg(RefinedProg.RType.CHOICE, 
                [RefinedProg(RefinedProg.RType.SEQ, 
                    [RefinedProg(RefinedProg.RType.REPEAT, RefinedProg(RefinedProg.RType.TP, [2, 0, 3])),
                    RefinedProg(RefinedProg.RType.TP, [2, 1, 4])]),
                 RefinedProg(RefinedProg.RType.SEQ, 
                    [RefinedProg(RefinedProg.RType.REPEAT, RefinedProg(RefinedProg.RType.TP, [2, 1, 4])),
                    RefinedProg(RefinedProg.RType.TP, [2, 0, 3])])
                    ]), 1)

        transition_graph = GraphParser("./examples/ps_reachability_bound/multiple_round_odd_sim.br").abscfg_parse()
        refined_prog = ProgramRefine(transition_graph).program_refine()
        print("The Expected Refined Program for the Multiple Path Odd While Program is: ", expected_refined_prog.prog_id(), expected_refined_prog.prog_signature())
        print("The Computed Refined Program for the Multiple Path Odd While Program is: ", refined_prog.prog_id(),refined_prog.prog_signature())

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
                        RefinedProg(RefinedProg.RType.TP, [2, 1, 4])]))], 1)

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
                        RefinedProg(RefinedProg.RType.TP, [3, 1, 5])]))], 1)

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
                        RefinedProg(RefinedProg.RType.TP, [8, 10]), 3),
                        RefinedProg(RefinedProg.RType.TP, [9, 11])]), 2),
                        RefinedProg(RefinedProg.RType.TP, [12, 2])])
                        , 1)

        print("The Reachability Bounds Expected for  Vertices in the Three Level Nested While Graph are: [1, 1, k, k/2, k/2, k] ")
        print("The Reachability Bounds Expected for  Vertices in the Three Level Nested While Graph are: [1, 1, k, k/2, k/2, k] ")

        transition_graph = GraphParser("./examples/ps_reachability_bound/three_nested_while.br").abscfg_parse()
        self.runner(transition_graph, refined_prog)

    def three_nested_while_II(self):
        refined_prog = RefinedProg(RefinedProg.RType.REPEAT, 
        RefinedProg(RefinedProg.RType.SEQ,
                [RefinedProg(RefinedProg.RType.TP, [1, 3]),
                RefinedProg(RefinedProg.RType.REPEAT, 
                    RefinedProg(RefinedProg.RType.SEQ,
                        [ RefinedProg(RefinedProg.RType.TP, [4, 6, 7]),
                        RefinedProg(RefinedProg.RType.REPEAT, 
                        RefinedProg(RefinedProg.RType.TP, [8, 10]), 3),
                        RefinedProg(RefinedProg.RType.TP, [9, 11])]), 2),
                        RefinedProg(RefinedProg.RType.TP, [12, 2])])
                        , 1)

        print("The Reachability Bounds Expected for  Vertices in the Three Level Nested While Graph are: [1, 1, k, k/2, k/2, k] ")
        print("The Reachability Bounds Expected for  Vertices in the Three Level Nested While Graph are: [1, 1, k, k/2, k/2, k] ")

        transition_graph = GraphParser("./examples/ps_reachability_bound/three_nested_while_II.br").abscfg_parse()
        self.runner(transition_graph, refined_prog)       
    
    def run_tests(self):
        self.test_seq()
        self.while_sim()
        # self.multiple_round_odd_sim()
        # self.multiple_round_single_sim()
        # self.while_two_counters()
        # self.three_nested_while()
        # self.three_nested_while_II()



tester = TestUnits(ProgramRefine)
tester.run_tests()

