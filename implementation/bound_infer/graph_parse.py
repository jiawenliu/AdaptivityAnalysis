from bound_infer import TransitionGraph, TransitionBound, DifferenceConstraint, VariableReachingBound
from adapt_search_refined import Graph, AdaptType, AdaptSearchAlgRefined
from adapt_estimate import AdaptEstimate
import argparse

class GraphParser(argparse.ArgumentParser):
    def __init__(self) -> None:
        super().__init__(description='Process the Abstract Control Flow Graph and Data Dependency Graph')
        self.add_argument('-i', '--inputfile',
        type=str, 
        default= "AdaptivityAnalysis/implementation/bound_infer/examples/seq.br", 
        help='The input file name')

        self.args = self.parse_args()


    def dcfg_parse(self):
        with open(self.args.inputfile, "r") as graphdata:
            n = int(graphdata.readline())
            query = [int(q) for q in graphdata.readline().strip("\n").split(",")]
            edges = [([int(v) for v in e.split(",")]) for e in graphdata.readline().split(";")]

            print(n, query, edges)
            return Graph(edges, [AdaptType(0)]*n, query)

    def abscfg_parse(self):
        with open(self.args.inputfile, "r") as graphdata:
            _ = [graphdata.readline() for _ in range(3)]
            n = int( graphdata.readline())
            edges = [[int(v) for v in e.split(",")] for e in graphdata.readline().split(";")[:-1]]
            transitions = []
            for l in graphdata.readlines():
                l1, dc, l2, v = l.split(";")
                (var, avar, c, ctype) = dc.split(",")
                print((var, avar, c, ctype))
                dc_type = DifferenceConstraint.DCType.RESET if ctype == "RESET" else DifferenceConstraint.DCType.INC if ctype == "INC" else DifferenceConstraint.DCType.DEC
                avar = None if avar == "" else avar
                c =  None if c == "" else int(c) if isinstance(c, int) else c
                transitions.append((int(l1), [ DifferenceConstraint(var, avar, c, dc_type) ], int(l2), [int(v)]))
    
            print(n, edges, transitions)
            return TransitionGraph(edges, transitions)

        pass