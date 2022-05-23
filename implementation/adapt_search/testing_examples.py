from adapt_search_naive import AdaptSearchAlg, AdaptType, Graph



# the example with only sequence, 
# Expected Adaptivity: 4
# Ouput Adaptivity: 4
def test_seq():
    weights = [AdaptType(1), AdaptType(1), AdaptType(1), AdaptType(1)]
    query = [1, 1, 1, 1]
    edges = [(0, 1), (1, 2), (2, 3)]
    adapt_search = AdaptSearchAlg(Graph(edges, weights, query))
    adapt_search.search_adapt()
    print("The Adaptivity From This Graph is: ", adapt_search.get_adapt())



# the example in if branch and only value dependency, 
# Expected Adaptivity: 2
# Ouput Adaptivity: 2
def test_if_val():
    weights = [AdaptType(1), AdaptType(1), AdaptType(1), AdaptType(1)]
    query = [1, 1, 1, 0]
    edges = [(0, 2), (1, 2), (1, 3)]
    adapt_search = AdaptSearchAlg(Graph(edges, weights, query))
    adapt_search.search_adapt()
    print("The Adaptivity From This Graph is: ", adapt_search.get_adapt())

# the example in if branch with both value and control dependency, 
# Expected Adaptivity: 2
# Ouput Adaptivity: 2
def test_if_val_ctl():
    weights = [AdaptType(1), AdaptType(1), AdaptType(1)]
    query = [1, 1, 1]
    edges = [(0, 2), (0, 1)]
    adapt_search = AdaptSearchAlg(Graph(edges, weights, query))
    adapt_search.search_adapt()
    print("The Adaptivity From This Graph is: ", adapt_search.get_adapt())

# the two-round example, 
# Expected Adaptivity: 2
# Ouput Adaptivity: 1 + k
def test_two_round():
    weights = [AdaptType(1), AdaptType("k"), AdaptType("k"), AdaptType(1)]
    query = [1, 1, 0, 0]
    edges = [(0, 1), (1, 2), (2,2), (2, 3)]
    adapt_search = AdaptSearchAlg(Graph(edges, weights, query))
    adapt_search.search_adapt()
    print("The Adaptivity From This Graph is: ", adapt_search.get_adapt())

# the example with while loop of multi-path from if branch (multi-path loop will result in different visiting times for
# verteices  belong to the same loop),  
# Expected Adaptivity: 1 + k
# Ouput Adaptivity: 1 + k/2 + k
def test_while_multipath_if():
    weights = [AdaptType(1), AdaptType("k/2"), AdaptType("k")]
    query = [1, 1, 1]
    edges = [(0, 1), (1, 2), (2, 1)]
    adapt_search = AdaptSearchAlg(Graph(edges, weights, query))
    adapt_search.search_adapt()
    print("The Adaptivity From This Graph is: ", adapt_search.get_adapt())


# the example with Nested loop and nested val Dep,  
# Expected Adaptivity: 1 + k + k!
# Ouput Adaptivity: 1 + k + k!
def test_while_nested():
    weights = [AdaptType(1), AdaptType("k"), AdaptType("k!")]
    query = [1, 1, 1]
    edges = [(0, 1), (0,2), (2, 1), (2, 2)]
    adapt_search = AdaptSearchAlg(Graph(edges, weights, query))
    adapt_search.search_adapt()
    print("The Adaptivity From This Graph is: ", adapt_search.get_adapt())

# the example with Nested loop and nested val Dep of multi-ath from nested loop,  
# Expected Adaptivity: 1 + 2 * k
# Ouput Adaptivity: 1 + k + k!
def test_while_multipath_nested():
    weights = [AdaptType(1), AdaptType("k!"), AdaptType("k")]
    query = [1, 1, 1]
    edges = [(0, 1), (1, 2), (2, 1)]
    adapt_search = AdaptSearchAlg(Graph(edges, weights, query))
    adapt_search.search_adapt()
    print("The Adaptivity From This Graph is: ", adapt_search.get_adapt())

# the example with Loop multi-varible Dependency  
# Expected Adaptivity: 1 + 2 * k
# Ouput Adaptivity: 1 + 3 * k
def test_while_multivar():
    weights = [AdaptType(1), AdaptType(1), AdaptType("k"), AdaptType("k"), AdaptType("k")]
    query = [1, 1, 1, 1, 1]
    edges = [(0, 2), (1, 2), (2, 3), (2, 4), (3, 2), (4, 2)]
    adapt_search = AdaptSearchAlg(Graph(edges, weights, query))
    adapt_search.search_adapt()
    print("The Adaptivity From This Graph is: ", adapt_search.get_adapt())


# the example with Nested loop and nested val Dep and multi-varibel Dependency 
# Expected Adaptivity: 1 +  k + k!
# Ouput Adaptivity: 1 + k + k!
def test_while_multivar_nested():
    weights = [AdaptType(1), AdaptType(1), AdaptType("k"), AdaptType("k!")]
    query = [1, 1, 1, 1]
    edges = [(0, 2), (0,3), (1, 2), (2, 2), (2, 3),(3, 2), (3, 3)]
    adapt_search = AdaptSearchAlg(Graph(edges, weights, query))
    adapt_search.search_adapt()
    print("The Adaptivity From This Graph is: ", adapt_search.get_adapt())

# the example with While loop and overlapped (or not overlapped, they produce the same graph) control and val Dep  Dependency 
# Expected Adaptivity: 1 +  k + k
# Ouput Adaptivity: 1 + k + k
def test_while_val_ctl():
    weights = [AdaptType(1), AdaptType(1), AdaptType("x"), AdaptType("x")]
    query = [1, 1, 1, 1]
    edges = [(0, 2), (0,3), (1, 2), (2, 2), (3, 2), (3, 3)]
    adapt_search = AdaptSearchAlg(Graph(edges, weights, query))
    adapt_search.search_adapt()
    print("The Adaptivity From This Graph is: ", adapt_search.get_adapt())

# the example with While loop,  Expected Adaptivity: 6
def test10():
    weights = [AdaptType(1.5), AdaptType(1), AdaptType(2), AdaptType(2)]
    query = [1, 1, 1, 1]
    edges = [(0, 1), (1, 2), (2, 3), (3, 2), (2, 2)]
    adapt_search = AdaptSearchAlg(Graph(edges, weights, query))
    adapt_search.search_adapt()
    print("The Adaptivity From This Graph is: ", adapt_search.get_adapt())

test_seq()
test_if_val()
test_if_val_ctl()
test_two_round()
test_while_multipath_if()
test_while_nested()
test_while_multipath_nested()
test_while_multivar()
test_while_multivar_nested()
test_while_val_ctl()
test10()


