

import enum
import math


class AdaptType:
    value = 0
    def __init__(self, value = 0) -> None:
        self.value = value
    def __add__(self, other):
        if (isinstance(self.value, str)) or isinstance(other.value, str):
            if (isinstance(self.value, int) and int(self.value) == 0):
                return other
            if (isinstance(other.value, int) and int(other.value) == 0):
                return self
            return AdaptType(str(self.value) + " + " + str(other.value))
        else:
            # print(self.value, other.value, " both are int")
            return AdaptType(self.value + other.value)

    def __radd__(self, other):
        if (isinstance(self.value, str)) or isinstance(other.value, str):
            if (isinstance(self.value, int) and int(self.value) == 0):
                return other
            if (isinstance(other.value, int) and int(other.value) == 0):
                return self
            return AdaptType(str(other.value) + " + " + str(self.value))
        else:
            # print(self.value, other.value, " both are int")
            return AdaptType(self.value + other.value)
    
    def __mul__(self, other):
        if (isinstance(self.value, str)) or isinstance(other.value, str):
            if (isinstance(self.value, int) and int(self.value) == 0) or (isinstance(other.value, int) and int(other.value) == 0):
                return AdaptType(0)
            if (isinstance(self.value, int) and int(self.value) == 1):
                return other
            if (isinstance(other.value, int) and int(other.value) == 1):
                return self
            return AdaptType("(" + str(other.value) + ") * (" + str(self.value) + ")")
        else:
            # print(self.value, other.value, " both are int")
            return AdaptType(self.value * (other.value))

    def adapt_max(self, other):
        if (isinstance(self.value, str)) and isinstance(other.value, str):
            if (isinstance(self.value, int) and int(self.value) == 0):
                return other
            if (isinstance(other.value, int) and int(other.value) == 0):
                return self
            return AdaptType("max(" + str(self.value) + ", " + str(other.value) + ")")
        elif (isinstance(self.value, str)) or (isinstance(other.value, str)):
            return self  if other.value == 0 else other if self.value == 0 else AdaptType("max(" + str(self.value) + ", " + str(other.value) + ")")
            # if other.value == 0:
        # elif (isinstance(other.value, str)) and self.value == 0:
        #     return other
        else:
            return AdaptType(max(self.value, other.value))

    def adapt_min(self, other):
        if (isinstance(self.value, str)) and isinstance(other.value, str):
            if (isinstance(self.value, int) and int(self.value) == 0):
                return self
            if (isinstance(other.value, int) and int(other.value) == 0):
                return other
            return AdaptType("min(" + str(self.value) + ", " + str(other.value) + ")")
        elif (isinstance(self.value, str)) or (isinstance(other.value, str)):
            return self  if other.value == 0 else other if self.value == 0 else AdaptType("min(" + str(self.value) + ", " + str(other.value) + ")")
            # if other.value == 0:
        # elif (isinstance(other.value, str)) and self.value == 0:
        #     return other
        else:
            return AdaptType(min(self.value, other.value))

    # def __lt__(self, other):
    #     if (self.value is int) and (other.value is int):
    #         return self.value < other.value

    # def __le__(self, other):
    #     return self.value <= other.value

    # def __eq__(self, other):
    #     return self.value == other.value

    # def __ne__(self, other):
    #     return self.value != other.value

    # def __gt__(self, other):
    #     return self.value > other.value

    # def __ge__(self, other):
    #     return self.value >= other.value

    # def __str__(self):
    #     return str(self.value)

class Graph:
    weights = [AdaptType(1), AdaptType(1)]
    query = [1, 1]
    edges = [(0, 1)]
    def __init__(self, edges = [(0, 1)],  weights = [AdaptType(1), AdaptType(1)],query = [1, 1]):
        self.weights = weights 
        self.query = query
        self.edges = edges
    
    def get_vertice_num(self):
        return len(self.weights)


class AdaptSearchAlg:
    class Edge:
        to = -1
        next = -1

        def __init__(self, to = -1, next = -1):
            self.to = to 
            self.next = next        


    def __init__(self, graph = Graph()):
        self.graph = graph
        self.vertex_no =graph.get_vertice_num()
        self.edges = []
        self.first_visit = [0]*self.vertex_no
        self.last_visit = [0]*self.vertex_no
        
        self.scc_adapt = [AdaptType(0)]* (self.vertex_no + 1)
        self.head = [-1]*self.vertex_no
        self.scc_stack = []
        self.scc_graph = [[]]*(self.vertex_no+1)
        self.scc_id = [0]*self.vertex_no
        self.scc_cnt = 0
        self.dfs_clock = 0
        self.adaptivity = AdaptType(0)
        

    def create_edges (self):
        for e in self.graph.edges:
            self.edges.append(self.Edge(e[1], self.head[e[0]]))
            # print(self.edges[-1].to, self.edges[-1].next)
            self.head[e[0]] = len(self.edges) - 1
            # print(self.head)

    def two_direction_dfs (self, u: int):
        self.dfs_clock += 1
        self.first_visit[u] = self.last_visit[u] = self.dfs_clock
        self.scc_stack.append(u)
        i = self.head[u]
        while i != -1:
            v = self.edges[i].to
            # print("in visiting vertex: ", u, "in the visiting #: ", self.dfs_clock, 
            #     ". before visit vertex ", v, "the first visit is: ", self.first_visit[u], 
            #         "the last visit is: ", self.last_visit[u])
            # print("the vertex #: ", v, "is assigned a SCC number or not", (not self.scc_id[v]))
            if not self.first_visit[v]:
                self.two_direction_dfs(v)
                self.last_visit[u] = min(self.last_visit[u], self.last_visit[v])
                # print("in the visiting #: ", self.dfs_clock, ". after visit vertex ", v, "the first visit is: ", self.first_visit[v], 
                #     "the last visit is: ", self.last_visit[v])
            elif not self.scc_id[v]:
                self.last_visit[u] = min(self.last_visit[u], self.first_visit[v])
                # print("in the visiting #: ", self.dfs_clock, "the visited vertex ", v, ", whoes first visit is: ", self.first_visit[v], 
                #     "the last visit is: ", self.last_visit[v])
            i = self.edges[i].next
        
        if self.first_visit[u] == self.last_visit[u]:
            self.scc_cnt += 1
            # print("new scc # : ", self.scc_cnt, "with vetices: ", self.scc_stack)
            while True:
                x = self.scc_stack.pop()
                # print(x)
                self.scc_id[x] = self.scc_cnt
                self.scc_adapt[self.scc_cnt] = (self.scc_adapt[self.scc_cnt] + self.graph.weights[x]) if self.graph.query[x] else self.scc_adapt[self.scc_cnt]
                # print(self.scc_adapt)
                if x == u:
                    break
            # print("new scc # : ", self.scc_cnt, "with vetices: ", self.scc_stack)
            # print(self.scc_id)

    def find_scc(self):
        self.scc_id = [0]*self.vertex_no
        self.first_visit = [0]*self.vertex_no
        self.scc_adapt = [AdaptType(0)]*(self.vertex_no + 1)
        self.scc_cnt = self.dfs_clock = 0
        for i in range(0, self.vertex_no):
            if not self.first_visit[i]:
                self.two_direction_dfs(i)
        print("The SCC adaptivity: ", list(map(lambda a: a.value, self.scc_adapt)))


    def build_scc(self):
        self.scc_graph = [[] for _ in range(self.vertex_no+1)]
        for u in range(0, self.vertex_no):
            print("vertex ", u, " is the head of edge # ", self.head[u], "to the node: ", self.edges[self.head[u]].to)
            i = self.head[u]
            print("vertex ", u, "belongs to the scc # ", self.scc_id[u])
            while i != -1:
                v = self.edges[i].to
                if not self.scc_id[u] == self.scc_id[v]:
                    self.scc_graph[self.scc_id[u]].append(self.scc_id[v])
                i = self.edges[i].next
        print("The SCC graph: ", self.scc_graph)

  
    # def bfs_adapt(self):
    #     bfs_q = []
    #     visited = [False]*(self.vertex_no + 1)
    #     self.adapt = [AdaptType(0)]* (self.scc_cnt+2)
    #     for j in range(1, self.scc_cnt+1):
    #         if not visited[j]:
    #             self.adapt[j] = self.scc_adapt[j]
    #             visited[j] = True 
    #             bfs_q.append(j)
    #             while bfs_q != []:
    #                 u = bfs_q.pop(0)
    #                 # print(u)
    #                 # visited[u] = False
    #                 for v in self.scc_graph[u]:
    #                     self.adapt[v] = (self.adapt[v].adapt_max(self.adapt[u] + self.scc_adapt[v]))
    #                     if not visited[v]:
    #                         visited[v] = True 
    #                         bfs_q.append(v)
    #     print("Adaptivity of each SCC: ", list(map(lambda a: a.value, self.adapt)) )

    def bfs_adapt(self):
        bfs_q = []
        visited = [False]*(self.vertex_no + 1)
        self.adapt = [AdaptType(0)]* (self.scc_cnt+2)
        if sum(self.graph.query) == 0:
            return
        start_v = min([i if q == 1 else self.graph.get_vertice_num() for (i, q) in enumerate((self.graph.query))])
        
        # for j in range(1, self.scc_cnt+1):
        #     # if visited[j]: 
        #     #     continue
        #     # visited[j] = True
        #     self.adapt[j] = self.scc_adapt[j] if self.adapt[j].value == 0 else self.adapt[j]
        #     bfs_q.append(j)
        #     # for i in range(1, self.scc_cnt+1):
        #     #     self.adapt[i] = self.scc_adapt[i] if self.scc_id[0] == i  else AdaptType(0)
        #     # bfs_q.append(self.scc_id[0])
        #     while bfs_q != []:
        #         u = bfs_q.pop(0)
        #         # print(u)
        #         visited[u] = False
        #         for v in self.scc_graph[u]:
        #             self.adapt[v] = (self.adapt[v].adapt_max(self.adapt[u] + self.scc_adapt[v]))
        #             if not visited[v]:
        #                 visited[v] = True 
        #                 bfs_q.append(v)
        for i in range(1, self.scc_cnt+1):
            self.adapt[i] = self.scc_adapt[i] if self.scc_id[start_v] == i  else AdaptType(0)
        bfs_q.append(self.scc_id[start_v])
        while bfs_q != []:
            u = bfs_q.pop(0)
            # print(u)
            visited[u] = False
            for v in self.scc_graph[u]:
                self.adapt[v] = (self.adapt[v].adapt_max(self.adapt[u] + self.scc_adapt[v]))
                if not visited[v]:
                    visited[v] = True 
                    bfs_q.append(v)
        print("Adaptivity of each SCC: ", list(map(lambda a: a.value, self.adapt)) )

    def print_adapt(self):
        print("The Adaptivity From This Graph is: ", self.get_adapt())
        query_num = AdaptType(0)
        for qn in [(AdaptType(self.graph.query[i]) * self.graph.weights[i]) for i in range(self.graph.get_vertice_num())]:
            query_num += qn
        print("The Total Query Number For This Graph is: ", query_num.value)
        print("The Estimated Generalization Error with an Optimial qurey computation Mechanism is O(", 
        (self.adaptivity  * AdaptType((math.sqrt(sum(self.graph.query))))).value, "/"+f"√N )")
        #  u"\u221A".encode('utf-8'), "(n) )")


    def get_adapt(self):
        return self.adaptivity.value
    
    def search_adapt(self):
        self.create_edges()
        self.find_scc()
        self.build_scc()
        self.bfs_adapt()
        for adapt_value in self.adapt:
            self.adaptivity = self.adaptivity.adapt_max(adapt_value)

# # the example with only sequence, 
# # Expected Adaptivity: 4
# # Ouput Adaptivity: 4
# def test_seq():
#     weights = [AdaptType(1), AdaptType(1), AdaptType(1), AdaptType(1)]
#     query = [1, 1, 1, 1]
#     edges = [(0, 1), (1, 2), (2, 3)]
#     adapt_search = AdaptSearchAlg(Graph(edges, weights, query))
#     adapt_search.search_adapt()
#     print("The Adaptivity From This Graph is: ", adapt_search.get_adapt())



# # the example in if branch and only value dependency, 
# # Expected Adaptivity: 2
# # Ouput Adaptivity: 2
# def test_if_val():
#     weights = [AdaptType(1), AdaptType(1), AdaptType(1), AdaptType(1)]
#     query = [1, 1, 1, 0]
#     edges = [(0, 2), (1, 2), (1, 3)]
#     adapt_search = AdaptSearchAlg(Graph(edges, weights, query))
#     adapt_search.search_adapt()
#     print("The Adaptivity From This Graph is: ", adapt_search.get_adapt())

# # the example in if branch with both value and control dependency, 
# # Expected Adaptivity: 2
# # Ouput Adaptivity: 2
# def test_if_val_ctl():
#     weights = [AdaptType(1), AdaptType(1), AdaptType(1)]
#     query = [1, 1, 1]
#     edges = [(0, 2), (0, 1)]
#     adapt_search = AdaptSearchAlg(Graph(edges, weights, query))
#     adapt_search.search_adapt()
#     print("The Adaptivity From This Graph is: ", adapt_search.get_adapt())

# # the two-round example, 
# # Expected Adaptivity: 2
# # Ouput Adaptivity: 1 + k
# def test_two_round():
#     weights = [AdaptType(1), AdaptType("k"), AdaptType("k"), AdaptType(1)]
#     query = [1, 1, 0, 0]
#     edges = [(0, 1), (1, 2), (2,2), (2, 3)]
#     adapt_search = AdaptSearchAlg(Graph(edges, weights, query))
#     adapt_search.search_adapt()
#     print("The Adaptivity From This Graph is: ", adapt_search.get_adapt())

# # the example with while loop of multi-path from if branch (multi-path loop will result in different visiting times for
# # verteices  belong to the same loop),  
# # Expected Adaptivity: 1 + k
# # Ouput Adaptivity: 1 + k/2 + k
# def test_while_multipath_if():
#     weights = [AdaptType(1), AdaptType("k/2"), AdaptType("k")]
#     query = [1, 1, 1]
#     edges = [(0, 1), (1, 2), (2, 1)]
#     adapt_search = AdaptSearchAlg(Graph(edges, weights, query))
#     adapt_search.search_adapt()
#     print("The Adaptivity From This Graph is: ", adapt_search.get_adapt())


# # the example with Nested loop and nested val Dep,  
# # Expected Adaptivity: 1 + k + k!
# # Ouput Adaptivity: 1 + k + k!
# def test_while_nested():
#     weights = [AdaptType(1), AdaptType("k"), AdaptType("k!")]
#     query = [1, 1, 1]
#     edges = [(0, 1), (0,2), (2, 1), (2, 2)]
#     adapt_search = AdaptSearchAlg(Graph(edges, weights, query))
#     adapt_search.search_adapt()
#     print("The Adaptivity From This Graph is: ", adapt_search.get_adapt())

# # the example with Nested loop and nested val Dep of multi-ath from nested loop,  
# # Expected Adaptivity: 1 + 2 * k
# # Ouput Adaptivity: 1 + k + k!
# def test_while_multipath_nested():
#     weights = [AdaptType(1), AdaptType("k!"), AdaptType("k")]
#     query = [1, 1, 1]
#     edges = [(0, 1), (1, 2), (2, 1)]
#     adapt_search = AdaptSearchAlg(Graph(edges, weights, query))
#     adapt_search.search_adapt()
#     print("The Adaptivity From This Graph is: ", adapt_search.get_adapt())

# # the example with Loop multi-varible Dependency  
# # Expected Adaptivity: 1 + 2 * k
# # Ouput Adaptivity: 1 + 3 * k
# def test_while_multivar():
#     weights = [AdaptType(1), AdaptType(1), AdaptType("k"), AdaptType("k"), AdaptType("k")]
#     query = [1, 1, 1, 1, 1]
#     edges = [(0, 2), (1, 2), (2, 3), (2, 4), (3, 2), (4, 2)]
#     adapt_search = AdaptSearchAlg(Graph(edges, weights, query))
#     adapt_search.search_adapt()
#     print("The Adaptivity From This Graph is: ", adapt_search.get_adapt())


# # the example with Nested loop and nested val Dep and multi-varibel Dependency 
# # Expected Adaptivity: 1 +  k + k!
# # Ouput Adaptivity: 1 + k + k!
# def test_while_multivar_nested():
#     weights = [AdaptType(1), AdaptType(1), AdaptType("k"), AdaptType("k!")]
#     query = [1, 1, 1, 1]
#     edges = [(0, 2), (0,3), (1, 2), (2, 2), (2, 3),(3, 2), (3, 3)]
#     adapt_search = AdaptSearchAlg(Graph(edges, weights, query))
#     adapt_search.search_adapt()
#     print("The Adaptivity From This Graph is: ", adapt_search.get_adapt())

# # the example with While loop and overlapped (or not overlapped, they produce the same graph) control and val Dep  Dependency 
# # Expected Adaptivity: 1 +  k + k
# # Ouput Adaptivity: 1 + k + k
# def test_while_val_ctl():
#     weights = [AdaptType(1), AdaptType(1), AdaptType("x"), AdaptType("x")]
#     query = [1, 1, 1, 1]
#     edges = [(0, 2), (0,3), (1, 2), (2, 2), (3, 2), (3, 3)]
#     adapt_search = AdaptSearchAlg(Graph(edges, weights, query))
#     adapt_search.search_adapt()
#     print("The Adaptivity From This Graph is: ", adapt_search.get_adapt())

# # the example with While loop,  Expected Adaptivity: 6
# def test10():
#     weights = [AdaptType(1.5), AdaptType(1), AdaptType(2), AdaptType(2)]
#     query = [1, 1, 1, 1]
#     edges = [(0, 1), (1, 2), (2, 3), (3, 2), (2, 2)]
#     adapt_search = AdaptSearchAlg(Graph(edges, weights, query))
#     adapt_search.search_adapt()
#     print("The Adaptivity From This Graph is: ", adapt_search.get_adapt())

# test_seq()
# test_if_val()
# test_if_val_ctl()
# test_two_round()
# test_while_multipath_if()
# test_while_nested()
# test_while_multipath_nested()
# test_while_multivar()
# test_while_multivar_nested()
# test_while_val_ctl()
# test10()


