from weight_estimate import WeightEstimate
from graph_parse import GraphParser
import time
from adapt_search import AdaptSearchAlgRefined
import argparse

class ArgParser(argparse.ArgumentParser):
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
            help='The input file name of the weight')


            self.add_argument('-e', '--example',
                type=str, 
                default= "./examples/seq.br", 
                help='The example adaptive data analysis program name')

            self.add_argument('-p', '--program',
                type=str, 
                default= "./examples/seq.br", 
                help='The adaptive data analysis program name')


            self.add_argument('-v', '--version',
                type=int, 
                default= 0, 
                help='The alternative versions of the adaptfun')


            self.args = self.parse_args()
            self.program = self.args.example.split("/")[-1]
            self.version = self.args.version



class Adaptfun:
    @staticmethod
    def adaptfun(program):
        dependency_file = "./dcfg/" + program
        abs_cfg_file = "./abscfg/" + program
        graph_parser = GraphParser(program, dependency_file, abs_cfg_file)
        dcf_graph = graph_parser.dcfg_parse()
        abscf_graph = graph_parser.abscfg_parse()

        start_time = time.time()
        weight_infer = WeightEstimate.ProgramBasedDependencyGraphWeightsEstimation(dcf_graph, abscf_graph)

        weight_infer.weight_estimate()
        weight_infer.print_weights()

        print("--- REACHABILITY BOUND COMPUTATION TIME: %s seconds ---" % (time.time() - start_time))

        adapt_search = AdaptSearchAlgRefined(weight_infer.graph)
        start_time = time.time()
        adapt_search.search_adapt()
        print("--- ADAPTIVITY COMPUTATION TIME: %s seconds ---" % (time.time() - start_time))
        adapt_search.print_adapt()  
        adapt_search.print_query_num()

    @staticmethod
    def adaptfun_a(program):
        dependency_file = "./dcfg/" + program
        abs_cfg_file = "./abscfg/" + program
        graph_parser = GraphParser(program, dependency_file, abs_cfg_file)
        dcf_graph = graph_parser.dcfg_parse()
        abscf_graph = graph_parser.abscfg_parse()

        start_time = time.time()
        weight_infer = WeightEstimate.ProgramBasedDependencyGraphWeightsEstimation(dcf_graph, abscf_graph)

        adapt_search = AdaptSearchAlgRefined(weight_infer.graph)
        start_time = time.time()
        adapt_search.search_adapt()
        print("--- ADAPTIVITY COMPUTATION TIME: %s seconds ---" % (time.time() - start_time))
        adapt_search.print_adapt()  
        adapt_search.print_query_num()

    def adaptfun_b(program):

        dependency_file = "./dcfg/" + program
        abs_cfg_file = "./abscfg/" + program
        graph_parser = GraphParser(program, dependency_file, abs_cfg_file)
        dcf_graph = graph_parser.dcfg_parse()
        abscf_graph = graph_parser.abscfg_parse()


        start_time = time.time()
        weight_infer = WeightEstimate.ProgramBasedDependencyGraphWeightsEstimation(dcf_graph, abscf_graph)
        weight_infer.weight_estimate_without_reachability_bound()

        adapt_search = AdaptSearchAlgRefined(weight_infer.graph)
        start_time = time.time()
        adapt_search.search_adapt()
        print("--- ADAPTIVITY COMPUTATION TIME: %s seconds ---" % (time.time() - start_time))
        adapt_search.print_adapt()  
        adapt_search.print_query_num()

    def adaptfun_c(program):
        
        dependency_file = "./dfg/" + program
        abs_cfg_file = "./abscfg/" + program
        graph_parser = GraphParser(program, dependency_file, abs_cfg_file)
        dcf_graph = graph_parser.dcfg_parse()
        abscf_graph = graph_parser.abscfg_parse()

        start_time = time.time()
        weight_infer = WeightEstimate.ProgramBasedDependencyGraphWeightsEstimation(dcf_graph, abscf_graph)
        # weight_infer.weight_estimate_without_reachability_bound()
        # weight_infer.print_weights()

        print("--- REACHABILITY BOUND COMPUTATION TIME: %s seconds ---" % (time.time() - start_time))
        adapt_search = AdaptSearchAlgRefined(weight_infer.graph)
        start_time = time.time()
        adapt_search.search_adapt()
        print("--- ADAPTIVITY COMPUTATION TIME: %s seconds ---" % (time.time() - start_time))
        adapt_search.print_adapt()  


    def __init__(self) -> None:
        pass
    
    def main(self):
        arg_parser = ArgParser()
        if( arg_parser.version == 0 ):
            print("--- ADAPTFUN ---")
            Adaptfun.adaptfun(arg_parser.program)

        elif( arg_parser.version == 1 ):
            print("--- ALTERNATIVE-A : THE LIGHT AND LOOSE REACHABILITY BOUND COMPUTATION ---")
            Adaptfun.adaptfun_a(arg_parser.program)

        elif( arg_parser.version == 2 ):
            print("--- ALTERNATIVE-B : NO REACHABILITY BOUND COMPUTATION ---")
            Adaptfun.adaptfun_b(arg_parser.program)

        elif( arg_parser.version == 3 ):
            print("--- ALTERNATIVE-C : NO CONTROL DEPENDENCY ANALYSIS ---")
            Adaptfun.adaptfun_c(arg_parser.program)
        else:
            print("--- INVALID VERSION NUMBER ---")



start_time = time.time()
adaptfun = Adaptfun()
adaptfun.main()
print("--- ADAPTIVITY ANALYSIS TIME: %s seconds ---" % (time.time() - start_time))

