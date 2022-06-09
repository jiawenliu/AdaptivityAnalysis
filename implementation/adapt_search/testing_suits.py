from adapt_search_naive import AdaptSearchAlg, AdaptType, Graph
from adapt_search_refined import AdaptSearchAlgRefined


class TestUnits:

    def __init__(self, ALG) -> None:
        self.ALG = ALG

    # the example with only sequence, 
    # Expected Adaptivity: 4
    # Ouput Adaptivity: 4
    def test_seq(self):
        weights = [AdaptType(1), AdaptType(1), AdaptType(1), AdaptType(1)]
        query = [1, 1, 1, 1]
        edges = [(0, 1), (1, 2), (2, 3)]

        adapt_search = self.ALG(Graph(edges, weights, query))
        adapt_search.search_adapt()
        print("The Adaptivity Expected for This Graph is: 4 ")
        print("The Adaptivity Calculated for This Graph is: ", adapt_search.get_adapt())



    # the example in if branch and only value dependency, 
    # Expected Adaptivity: 2
    # Ouput Adaptivity: 2
    def test_if_val(self):
        weights = [AdaptType(1), AdaptType(1), AdaptType(1), AdaptType(1)]
        query = [1, 1, 1, 0]
        edges = [(0, 2), (1, 2), (1, 3)]
        adapt_search = self.ALG(Graph(edges, weights, query))
        adapt_search.search_adapt()
        print("The Adaptivity Expected for This Graph is: 2 ")
        print("The Adaptivity From This Graph is: ", adapt_search.get_adapt())

    # the example in if branch with both value and control dependency, 
    # Expected Adaptivity: 2
    # Ouput Adaptivity: 2
    def test_if_val_ctl(self):
        weights = [AdaptType(1), AdaptType(1), AdaptType(1)]
        query = [1, 1, 1]
        edges = [(0, 2), (0, 1)]
        adapt_search = self.ALG(Graph(edges, weights, query))
        adapt_search.search_adapt()
        print("The Adaptivity Expected for This Graph is: 2 ")
        print("The Adaptivity From This Graph is: ", adapt_search.get_adapt())

    # the two-round example, 
    # Expected Adaptivity: 2
    # Ouput Adaptivity: 1 + k
    def test_two_round(self):
        weights = [AdaptType(1), AdaptType("k"), AdaptType("k"), AdaptType(1)]
        query = [1, 1, 0, 0]
        edges = [(0, 1), (1, 2), (2,2), (2, 3)]
        adapt_search = self.ALG(Graph(edges, weights, query))
        adapt_search.search_adapt()
        print("The Adaptivity Expected for This Graph is: 2 ")
        print("The Adaptivity From This Graph is: ", adapt_search.get_adapt())

    # the multiple-round example, 
    # Expected Adaptivity: k
    def test_multiple_round(self):
        weights = [AdaptType(1),  AdaptType(1), AdaptType("k"), AdaptType("k"), AdaptType("k"), AdaptType("k")]
        query = [0, 1, 0, 1, 1, 1]
        edges = [(0, 2), (2, 3), (1, 3), (1, 4), (1, 5), (3, 5), (5, 3), (5, 4)]
        adapt_search = self.ALG(Graph(edges, weights, query))
        adapt_search.search_adapt()
        print("The Adaptivity Expected for This temp Graph is: k ")
        print("The Adaptivity From This Graph is: ", adapt_search.get_adapt())

    # the example with while loop of multi-path from if branch (multi-path loop will result in different visiting times for
    # verteices  belong to the same loop),  
    # Expected Adaptivity: 1 + k
    # Ouput Adaptivity: 1 + k/2 + k
    def test_while_multipath_if(self):
        weights = [AdaptType(1), AdaptType("k/2"), AdaptType("k")]
        query = [1, 1, 1]
        edges = [(0, 1), (1, 2), (2, 1)]
        adapt_search = self.ALG(Graph(edges, weights, query))
        adapt_search.search_adapt()
        print("The Adaptivity Expected for This Graph is: 1 + k ")
        print("The Adaptivity From This Graph is: ", adapt_search.get_adapt())

    # the example with Nested loop and nested val Dep,  
    # Expected Adaptivity: 2 + k!
    # Ouput Adaptivity: 1 + k + k!
    def test_while_nested(self):
        weights = [AdaptType(1), AdaptType("k"), AdaptType("k!")]
        query = [1, 1, 1]
        edges = [(0, 1), (0,2), (2, 1), (2, 2)]
        adapt_search = self.ALG(Graph(edges, weights, query))
        adapt_search.search_adapt()
        print("The Adaptivity Expected for This Graph is: 2 + k! ")
        print("The Adaptivity From This Graph is: ", adapt_search.get_adapt())

    # the nested while example, 
    # Expected Weights: [1, 1, k, k, k, k^2, k^2]
    def test_simple_nested2(self):
        weights = [AdaptType(1), AdaptType(1), AdaptType("k"), AdaptType("k"), AdaptType("k"), AdaptType("k * k"), AdaptType("k * k")]
        query = [0, 1, 0, 0, 1, 0, 1]
        edges = [(0, 2), (1, 4), (1, 6), (2, 2), (2, 3), (3, 5), (5, 5), (6, 4), (6, 6)]

        adapt_search = self.ALG(Graph(edges, weights, query))
        adapt_search.search_adapt()
        print("The Adaptivity Expected for Simple Nested While Algorithm is: 2 + k * k ")
        print("The Adaptivity From This Graph is: ", adapt_search.get_adapt())

    # the example with Nested loop and nested val Dep of multi-ath from nested loop,  
    # Expected Adaptivity: 1 + 2 * k
    # Ouput Adaptivity: 1 + k + k!
    def test_while_multipath_nested(self):
        weights = [AdaptType(1), AdaptType("k!"), AdaptType("k")]
        query = [1, 1, 1]
        edges = [(0, 1), (1, 2), (2, 1)]
        adapt_search = self.ALG(Graph(edges, weights, query))
        adapt_search.search_adapt()
        print("The Adaptivity Expected for This Graph is: 1 + 2 * k ")
        print("The Adaptivity From This Graph is: ", adapt_search.get_adapt())

    # the example with Loop multi-varible Dependency  
    # Expected Adaptivity: 1 + 2 * k
    # Ouput Adaptivity: 1 + 3 * k
    def test_while_multivar(self):
        weights = [AdaptType(1), AdaptType(1), AdaptType("k"), AdaptType("k"), AdaptType("k")]
        query = [1, 1, 1, 1, 1]
        edges = [(0, 2), (1, 2), (2, 3), (2, 4), (3, 2), (4, 2)]
        adapt_search = self.ALG(Graph(edges, weights, query))
        adapt_search.search_adapt()
        print("The Adaptivity Expected for This Graph is: 1 + 2 * k ")
        print("The Adaptivity From This Graph is: ", adapt_search.get_adapt())


    # the example with Nested loop and nested val Dep and multi-varibel Dependency 
    # Expected Adaptivity: 1 +  k + k!
    # Ouput Adaptivity: 1 + k + k!
    def test_while_multivar_nested(self):
        weights = [AdaptType(1), AdaptType(1), AdaptType("k"), AdaptType("k!")]
        query = [1, 1, 1, 1]
        edges = [(0, 2), (0,3), (1, 2), (2, 2), (2, 3),(3, 2), (3, 3)]
        adapt_search = self.ALG(Graph(edges, weights, query))
        adapt_search.search_adapt()
        print("The Adaptivity Expected for This Graph is: 1 + k + k! ")
        print("The Adaptivity From This Graph is: ", adapt_search.get_adapt())

    # the example with While loop and overlapped (or not overlapped, they produce the same graph) control and val Dep  Dependency 
    # Expected Adaptivity: 1 +  2 * x
    # Ouput Adaptivity: 1 + k + k
    def test_while_val_ctl(self):
        weights = [AdaptType(1), AdaptType(1), AdaptType("x"), AdaptType("x")]
        query = [1, 1, 1, 1]
        edges = [(0, 2), (0,3), (1, 2), (2, 2), (3, 2), (3, 3)]
        adapt_search = self.ALG(Graph(edges, weights, query))
        adapt_search.search_adapt()
        print("The Adaptivity Expected for This Graph is: 1 + 2 * x")
        print("The Adaptivity From This Graph is: ", adapt_search.get_adapt())

    # the example with While loop,  Expected Adaptivity: 6
    def test10(self):
        weights = [AdaptType(1.5), AdaptType(1), AdaptType(2), AdaptType(2)]
        query = [1, 1, 1, 1]
        edges = [(0, 1), (1, 2), (2, 3), (3, 2), (2, 2)]
        adapt_search = self.ALG(Graph(edges, weights, query))
        adapt_search.search_adapt()
        print("The Adaptivity Expected for This Graph is: 6 ")
        print("The Adaptivity From This Graph is: ", adapt_search.get_adapt())
    
    def run_tests(self):
        self.test_seq()
        self.test_if_val()
        self.test_if_val_ctl()
        self.test_two_round()
        self.test_while_multipath_if()
        self.test_while_nested()
        self.test_while_multipath_nested()
        self.test_while_multivar()
        self.test_while_multivar_nested()
        self.test_while_val_ctl()
        self.test10()
        self.test_simple_nested2()




# test_naive = TestUnits(AdaptSearchAlg)
# test_naive.run_tests()


test_refined = TestUnits(AdaptSearchAlgRefined)
test_refined.run_tests()
test_refined.test_multiple_round()
