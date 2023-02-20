from weight_estimate import WeightEstimate
from graph_parse import GraphParser
import time
from adapt_search import AdaptSearchAlgRefined


class Adaptfun:
    @staticmethod
    def adaptfun_B():
        
        dependency_file = "./dcfg/" + self.program
        abs_cfg_file = "./abscfg/" + self.program
        graph_parser = GraphParser(program, dependency_graph, abs_cfg_graph)
        dcf_graph = graph_parser.dcfg_parse()
        abscf_graph = graph_parser.abscfg_parse()

        start_time = time.time()
        weight_infer = WeightEstimate.ProgramBasedDependencyGraphWeightsEstimation(dcf_graph, abs_transition_graph)
        # dcf_graph.weights = [AdaptType(1)]* dcf_graph.get_vertice_num()
        weight_infer.weight_estimate_without_reachability_bound()
        print("--- ALTERNATIVE-B : NO REACHABILITY BOUND COMPUTATION ---")
        adapt_search = AdaptSearchAlgRefined(weight_infer.graph)
        start_time = time.time()
        adapt_search.search_adapt()
        print("--- ADAPTIVITY COMPUTATION TIME: %s seconds ---" % (time.time() - start_time))
        adapt_search.print_adapt()  

    def __init__(self) -> None:
        # self.filename = filename
        pass
    
    def main(self):

        Adaptfun.adaptfun(version)

start_time = time.time()
runner = Adaptfun()
runner.main()
print("--- ADAPTIVITY ANALYSIS TIME: %s seconds ---" % (time.time() - start_time))
