from weight_estimate import WeightEstimate
from graph_parse import GraphParser
import time
from adapt_search import AdaptSearchAlgRefined


class Adaptfun:
    @staticmethod
    def adaptfun_A(dcf_graph, abs_transition_graph):
        start_time = time.time()
        weight_infer = WeightEstimate.ProgramBasedDependencyGraphWeightsEstimation(dcf_graph, abs_transition_graph)

        print("--- ALTERNATIVE-A : THE LIGHT AND LOOSE REACHABILITY BOUND COMPUTATION ---")

        adapt_search = AdaptSearchAlgRefined(weight_infer.graph)
        start_time = time.time()
        adapt_search.search_adapt()
        print("--- ADAPTIVITY COMPUTATION TIME: %s seconds ---" % (time.time() - start_time))
        adapt_search.print_adapt()  

    def __init__(self) -> None:
        # self.filename = filename
        pass
    
    def main(self):
        graph_parser = GraphParser()
        dcf_graph = graph_parser.dcfg_parse()
        abscf_graph = graph_parser.abscfg_parse()
        Adaptfun.adaptfun(dcf_graph, abscf_graph)

start_time = time.time()
runner = Adaptfun()
runner.main()
print("--- ADAPTIVITY ANALYSIS TIME: %s seconds ---" % (time.time() - start_time))
