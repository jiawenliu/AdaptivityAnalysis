from bound_infer import TransitionGraph, TransitionBound, DifferenceConstraint
from adapt_search_refined import Graph, AdaptType, AdaptSearchAlgRefined
import argparse

class GraphParser(argparse.ArgumentParser):
    def __init__(self, example = None) -> None:
        super().__init__(description='Process the Abstract Control Flow Graph and Data Dependency Graph')
        self.add_argument('-d', '--dcfg',
        type=str, 
        default= "AdaptivityAnalysis/implementation/bound_infer/examples_dcfg/seq.br", 
        help='The input file name for the Data Control Flow Graph')

        self.add_argument('-a', '--abs_cfg',
        type=str, 
        default= "AdaptivityAnalysis/implementation/bound_infer/examples_abscfg/seq.br", 
        help='The input file name for the Abstract Control Flow Graph')

        self.add_argument('-e', '--example',
        type=str, 
        default= "seq.br", 
        help='The example name for the Abstract Control Flow Graph')

        if example:
            self.example_path = example
        else:
            self.args = self.parse_args()
            self.example_path = self.args.example

        # Just for simplicity of testing, using the same name in different folder
        # Will be removed when lauching
        # self.args.dcfg = "./dcfg/" + self.args.example[11:]
        # self.args.abs_cfg = "./abscfg/" + self.args.example[11:]
        # self.example_path = self.args.example
        self.dcfg_file = "./dcfg/" + self.example_path[11:]
        self.abs_cfg_file = "./abscfg/" + self.example_path[11:]


    def dcfg_parse(self):
        with open(self.dcfg_file, "r") as graphdata:
            n = int(graphdata.readline())
            query = [int(q) for q in graphdata.readline().strip("\n").split(",")]
            edges = [([int(v) for v in e.split(",")]) for e in graphdata.readline().split(";")]

            print(n, query, edges)
            return Graph(edges, [AdaptType(0)]*n, query)

    def blockl_to_lvar(self):
        pass

    def abscfg_parse(self):
        with open(self.abs_cfg_file, "r") as graphdata:
            # _ = [graphdata.readline() for _ in range(3)]
            n = int( graphdata.readline())
            edges = [[(n - 1) if int(v) == -1 else int(v) for v in e.split(",")] for e in graphdata.readline().split(";")[:-1]]
            transitions = []
            for l in graphdata.readlines():
                l1, dc, l2, v = l.split(";")               
                if dc == "":
                    dc_set = []
                    v_set = [int(v)]
                else:
                    v_set = [int(v)]
                    (var, avar, c, ctype) = dc.split(",")
                    print((var, avar, c, ctype))
                    dc_type = DifferenceConstraint.DCType.RESET if ctype == "RESET" else DifferenceConstraint.DCType.INC if ctype == "INC" else DifferenceConstraint.DCType.DEC if ctype == "DEC" else DifferenceConstraint.DCType.ASUM
                    avar = None if avar == "" else avar
                    c =  None if c == "" else int(c) if isinstance(c, int) else c
                    dc_set = [DifferenceConstraint(var, avar, c, dc_type)]
                transitions.append((int(l1), dc_set, int(l2), v_set))
            transitions.sort(key=lambda y: y[0]) 
            edges.sort(key=lambda y: y[0])   
            print(n, edges, transitions)
            return TransitionGraph(edges, transitions)

        pass