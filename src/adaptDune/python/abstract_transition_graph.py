import enum
from collections import defaultdict

class DifferenceConstraint:
    class DCType(enum.Enum):
        DEC = 1
        INC = 2
        RESET = 3
        ASUM = 4
        WHILE = 5
        IF = 6

    dc_type = 1
    def __init__(self, var = None, dc_var = None, dc_const = None, dc_type = 1) -> None:
        self.var = None if dc_type == self.DCType.ASUM else var 
        self.dc_var = dc_var
        self.dc_const = dc_const
        self.dc_type = dc_type
        self.dc_bexpr = var if dc_type == self.DCType.ASUM else None
        self.transition_type = "WHILE" if self.dc_bexpr and self.dc_bexpr.startswith("WHILE:") else "IF" if self.dc_bexpr else "ASN"
        pass

    def get_var(self):
        return self.var 

    def is_reset(self):
        return self.dc_type == 3
    
    def is_inc(self):
        return self.dc_type == 2

    def is_dec(self):
        return self.dc_type == self.DCType.DEC

    def is_asum(self):
        return self.dc_type == self.DCType.ASUM

    def get_inc_value(self):
        return self.inc_value

    def get_dec_value(self):
        return self.dec_value
    
    def get_asum(self):
        return self.dc_bexpr


class DirectedGraph:
    def __init__(self, vertices_num = 1, edges = []) -> None:
        self.vertices_num = vertices_num
        self.edges = edges
        # default dictionary to store graph
        self.graph = defaultdict(list)
        self.time = 0
        self.scc_ids = [-1] * (self.vertices_num)
        self.scc_cnt = -1
        self.edge_indices = defaultdict(int)
        self.build_edges()
        self.build_edge_indices()
        pass

    def build_edges(self):
        for (u, v) in self.edges:
            self.graph[u].append(v)
    
    def build_edge_indices(self):
        for index, (u, v) in enumerate(self.edges):
            self.edge_indices[str(u) + "->" + str(v)] = index

    def search_path(self, st, dest):
        r = []
        self.build_edge_indices()
        visited = [0] * self.vertices_num
        def dfs(c, path):
            if c == dest:
                r.append(path)
                return 
            else:
                for next_v in self.graph[c]:
                    if not visited[next_v]:
                        visited[next_v] = 1
                        dfs(next_v, path + [self.edge_indices[str(c) + "->" + str(next_v)]])
                        visited[next_v] = 0
        dfs(st, [st])
        return r

    def is_in_scc(self, edge):
        if self.scc_cnt == -1:
            self.build_edges()
            self.build_scc()
        (u, v) = edge
        return self.scc_ids[u] == self.scc_ids[v]
    
    def scc_dfs(self,u, low, disc, stackMember, st):

        # Initialize discovery time and low value
        disc[u] = self.time
        low[u] = self.time
        self.time += 1
        stackMember[u] = True
        st.append(u)

        # Go through all vertices adjacent to this
        for v in self.graph[u]:
            
            # If v is not visited yet, then recur for it
            if disc[v] == -1 :
            
                self.scc_dfs(v, low, disc, stackMember, st)

                # Check if the subtree rooted with v has a connection to
                # one of the ancestors of u
                # Case 1 (per above discussion on Disc and Low value)
                low[u] = min(low[u], low[v])
                        
            elif stackMember[v] == True:

                '''Update low value of 'u' only if 'v' is still in stack
                (i.e. it's a back edge, not cross edge).
                Case 2 (per above discussion on Disc and Low value) '''
                low[u] = min(low[u], disc[v])

        # head node found, pop the stack and print an SCC
        w = -1 #To store stack extracted vertices
        if low[u] == disc[u]:
            self.scc_cnt += 1
            while w != u:
                w = st.pop()
                # print (w, end=" ")
                self.scc_ids[w] = self.scc_cnt
                stackMember[w] = False
                
            # print()
   

    #The function to do DFS traversal.
    # It uses recursive scc_dfs()
    def build_scc(self):

        # Mark all the vertices as not visited
        # and Initialize parent and visited,
        # and ap(articulation point) arrays
        disc = [-1] * (self.vertices_num)
        low = [-1] * (self.vertices_num)
        stackMember = [False] * (self.vertices_num)
        st =[]
        

        # Call the recursive helper function
        # to find articulation points
        # in DFS tree rooted with vertex 'i'
        for i in range(self.vertices_num):
            if disc[i] == -1:
                self.scc_dfs(i, low, disc, stackMember, st)  
     


#TODO: build Base Father Graph Calss
#Inherit the Transition Graph and the Data-Flow Graph from the Command Graph Class
class TransitionGraph(DirectedGraph):
    # locations = [0, 1]
    ctl_edges = [(0, 1), (1, 1)]
    transitions = [(0, [DifferenceConstraint("x", None, "k", DifferenceConstraint.DCType.RESET)], 1, [0]),
    (1, [DifferenceConstraint("x", None, "1", DifferenceConstraint.DCType.DEC)], 1, [1, 2])]

    def __init__(self, 
    edges=[(0, 1), (1, 1)], 
    transitions=[(0, [DifferenceConstraint("x", None, "k", DifferenceConstraint.DCType.RESET)], 1, [0]),
    (1, [DifferenceConstraint("x", None, "1", DifferenceConstraint.DCType.DEC)], 1, [1, 2])],
    vertex_num = None
    ):

        super().__init__(vertex_num if vertex_num else (max(map(lambda x: max(x), edges)) + 1), edges)
        self.ctl_edges = edges
        self.transitions = transitions
        self.transition_id = defaultdict(int)
    
    def transition_id_lookup(self):
        for id, transition in enumerate(self.transitions):
            # print(id, transition)
            l1,_,l2,_ = transition
            self.transition_id[str(l1) + "->" + str(l2)] = id






