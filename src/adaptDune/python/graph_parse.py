from abstract_transition_graph import TransitionGraph, DifferenceConstraint
from adapt_lib import AdaptType, Graph
import argparse

class GraphParser(argparse.ArgumentParser):
    def __init__(self, example = None) -> None:
        super().__init__(description='Process the Abstract Control Flow Graph and Data Dependency Graph')
        self.add_argument('-d', '--dcfg',
        type=str, 
        default= "./dcfg/seq.br", 
        help='The input file name for the Data Control Flow Graph')

        self.add_argument('-a', '--abs_cfg',
        type=str, 
        default= "./abscfg/seq.br", 
        help='The input file name for the Abstract Control Flow Graph')

        self.add_argument('-w', '--weight',
        type=str, 
        default= "./weight/seq.br", 
        help='The input file name of the Weight')


        self.add_argument('-e', '--example',
        type=str, 
        default= "./examples/seq.br", 
        help='The example name for the Abstract Control Flow Graph')


        self.args = self.parse_args()
        self.example_name = self.args.example.split("/")[-1]
        self.dcfg_file = "./dcfg/" + self.example_name
        self.abs_cfg_file = "./abscfg/" + self.example_name

    def weight_parse(self):
        with open(self.dcfg_file, "r") as weightdata:
            return [int(l) if isinstance(l, int) else l.strip("\n") for l in weightdata.readlines()]

    def dcfg_parse(self):
        with open(self.dcfg_file, "r") as graphdata:
            n = int(graphdata.readline())
            query = [int(q) for q in graphdata.readline().strip("\n").split(",")[:-1]]
            edges = [([int(v) for v in e.split(",")]) for e in graphdata.readline().split(";")[:-1]]
            weights_line = graphdata.readline()
            if weights_line:
                weights = [AdaptType(int(l)) if isinstance(l, int) else AdaptType(l) for l in weights_line.strip("\n").split(",")]
                # print("The Input DCFG: ", n, query, edges, weights)
                return Graph(edges, weights, query)               
            else:
                # print("The Input DCFG: ", n, query, edges)
                return Graph(edges, None, query)

    def abscfg_parse(self):
        with open(self.abs_cfg_file, "r") as graphdata:
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
                    # print((var, avar, c, ctype))
                    dc_type = DifferenceConstraint.DCType.RESET if ctype == "RESET" else DifferenceConstraint.DCType.INC if ctype == "INC" else DifferenceConstraint.DCType.DEC if ctype == "DEC" else DifferenceConstraint.DCType.ASUM
                    avar = None if avar == "" else avar
                    c =  None if c == "" else int(c) if isinstance(c, int) else c
                    dc_set = [DifferenceConstraint(var, avar, c, dc_type)]
                transitions.append((int(l1), dc_set, int(l2), v_set))
            transitions.sort(key=lambda y: y[0]) 
            edges.sort(key=lambda y: y[0])   
            # print("The Input ABS Transition Graph: ", n, edges, transitions)
            return TransitionGraph(edges, transitions, n)

        pass