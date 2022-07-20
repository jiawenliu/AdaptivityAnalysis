from adapt_estimate import AdaptEstimate
from graph_parse import GraphParser


class Runner:
    def __init__(self) -> None:
        # self.filename = filename
        pass
    
    def main(self):
        graph_parser = GraphParser()
        dcf_graph = graph_parser.dcfg_parse()
        abscf_graph = graph_parser.abscfg_parse()
        AdaptEstimate.adapt_estimate(dcf_graph, abscf_graph)

runner = Runner()
runner.main()

class TestUnits:

    def __init__(self, ALG) -> None:
        self.ALG = ALG

    # the example with Simple sequence, 
    # Expected Weights: [1,1,1,1]
    # Expected Adaptivity: 4
    def seq(self):
        AdaptEstimate.adapt_estimate(dcf_graph, abscf_graph)
        print("The Reachability Bounds Expected for  Vertices in Pure Sequence Graph are: [1,1,1,1] ")
        print("The Adaptivity Expected for Simple Seq Algorithm is: 4 ")


    # the example with Simple sequence, 
    # Expected Weights: [1,1,1,1]
    # Expected Adaptivity: 4
    def seq_multivar(self):
        print("The Reachability Bounds Expected for  Vertices in Pure Sequence Graph are: [1,1,1,1] ")
        print("The Adaptivity Expected for Simple Seq Algorithm is: 4 ")

    # the example in if branch and only value dependency, 
    # Expected Adaptivity: 2
    # Ouput Adaptivity: 2
    def if_valdep(self):
        print("The Reachability Bounds Expected for Mutli-path Dependency in If branch Algorithm are: [1,1,1,1] ")
        print("The Adaptivity Expected for Mutli-path Dependency in If branch Algorithm is: 2 ")


    def if_ctldep(self):
        print("The Reachability Bounds Expected for  Vertices in If Branch with Control Dependency Graph are: [1, 1, 1, 1] ")
        print("The Adaptivity Expected for Mutli-path Dependency in If branch Algorithm is: 3 ")

    # the example with simple Loop sequence, 
    # Expected Weights: [1, 1, 100, 100, 100]
    def while_valdep(self):
        print("The Reachability Bounds Expected for  Vertices in Simple while are: [1, 1, 100, 100, 100] ")

        print("The Adaptivity Expected for Simple While Algorithm is: 100 ")

    # the example with multi path Loop sequence, 
    def while_multipath(self):
        print("The Reachability Bounds Expected for  Vertices in while with If Banch inside are: [1, 1, k, k/2, k/2, k] ")

        print("The Adaptivity Expected for while with If Banch (Multi-Path While Loop) Algorithm is: 1 + k/2 + k/2")
        # print("The Adaptivity From This Graph is: ", adapt_search.get_adapt())

    # the example with multi path Loop sequence, 
    def while_multivaldep(self):
        print("The Reachability Bounds Expected for Vertices in while Multi Variable Dependency: [1, 1, 1, k, k, k, k] ")

        print("The Adaptivity Expected for while with (Multi Variable Dependency) Algorithm is: 1 + 2 * k")
        # print("The Adaptivity From This Graph is: ", adapt_search.get_adapt())

    # the example with simple Loop sequence, 
    # Expected Weights: [1, 1, INF, INF]
    def while_valctldep(self):
        print("The Reachability Bounds Expected for  Vertices in While with Control and Value Dependency Overlapped are: [1, 1, INF, INF] ")
        # print("The Reachability Bounds Calculated for Vertices in This Graph are: ", bound_infer.get_weights())
        
        # adapt_search = AdaptSearchAlgRefined(bound_infer.graph)
        # adapt_search.search_adapt()
        print("The Adaptivity Expected for While with Control and Value Dependency Overlapped Algorithm is: INF ")

    # the example with multi path Loop sequence, 
    def while_multipath_ctldep(self):

        # AdaptEstimate.adapt_estimate(Graph(edges, weights, query), TransitionGraph(ctl_edges, transitions))
        print("The Reachability Bounds Expected for  Vertices in while with If Banch inside are: [1, 1, Q, Q, Q, 1] ")

        print("The Adaptivity Expected for while with If Banch (Multi-Path While Loop) Algorithm is: 2 + Q")
        # print("The Adaptivity From This Graph is: ", adapt_search.get_adapt())


    # the nested while example, 
    # Expected Weights: [1, 1, k, k, k, k^2, k^2]
    def nested_while_valdep(self):
        # weights = [AdaptType(1), AdaptType(1), AdaptType("k"), AdaptType("k"), AdaptType("k"), AdaptType("k * k"), AdaptType("k * k")]
        # bound_infer = self.ALG(Graph(edges, weights, query), TransitionGraph(ctl_edges, transitions))
        # bound_infer.attach_weights()
        print("The Reachability Bounds Expected for  Vertices in the Simple Nested While Graph are: [1, 1, k, k, k, k^2, k^2] ")
        print("The Adaptivity Expected for Simple Nested While Algorithm is: 2 + k * k ")
        # print("The Adaptivity From This Graph is: ", adapt_search.get_adapt())

    # the nested while example, 
    # Expected Weights: [1, 1, k, k, k, k^2, k^2]
    def nested_while_recursivevaldep(self):

        # bound_infer = self.ALG(Graph(edges, weights, query), TransitionGraph(ctl_edges, transitions))
        # bound_infer.attach_weights()
        print("The Reachability Bounds Expected for  Vertices in the Simple Nested While Graph are: [1, 1, k, k, k^2, k^2, k^2] ")
        print("The Adaptivity Expected for Simple Nested While Algorithm is: 1 + 2 * k * k ")
        # print("The Adaptivity From This Graph is: ", adapt_search.get_adapt())


    # the nested while example, 
    # Expected Weights: [1, 1, k, k, k, k^2, k^2]
    def nested_while_multivaldep(self):
        print("The Reachability Bounds Expected for  Vertices in the Simple Nested While Graph are: [1, 1, k, k, k, k^2, k^2] ")
        print("The Adaptivity Expected for Simple Nested While Algorithm is: 1 + k + k * k ")
        # print("The Adaptivity From This Graph is: ", adapt_search.get_adapt())


    # the two-round example, 
    # Expected Weights: [1, 1, k, k, k, 1]
    # i.e., [AdaptType(1), AdaptType(1), AdaptType("k"), AdaptType("k"), AdaptType("k"), AdaptType(1)]
    def two_round(self):
        print("The Reachability Bounds Expected for  Vertices in the Two Round Graph are: [1, 1, k, k, k, 1] ")
        print("The Adaptivity Expected for Two Round Algorithm is: 2 ")


    # the multiple-round example, 
    # Expected Weights: [1, 1, k, k, k]
    # i.e., weights = [AdaptType(1),  AdaptType(1), AdaptType("k"), AdaptType("k"), AdaptType("k")]
    def multiple_round(self):
        print("The Reachability Bounds Expected for  Vertices in the Multiple Round Graph are: [1, 1, k, k, k] ")
        print("The Adaptivity Expected for multiple Round Algorithm is: k ")




