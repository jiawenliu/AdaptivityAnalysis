import enum
from collections import defaultdict
from adapt_search_refined import Graph, AdaptType


class DifferenceConstraint:
    class DCType(enum.Enum):
        DEC = 1
        INC = 2
        RESET = 3

    var = ""
    dc_var = ""
    dc_const = ""
    # dec_dc = ()
    # reset_dc = ()
    # inc_dc = ()
    dc_type = 1
    def __init__(self, var="x", dc_var = None, dc_const = None, dc_type = 1) -> None:
        self.var = var 
        self.dc_var = dc_var
        self.dc_const = dc_const
        self.dc_type = dc_type
        pass

    def get_var(self):
        return self.var 

    def is_reset(self):
        return self.dc_type == 3
    
    def is_inc(self):
        return self.dc_type == 2

    def is_dec(self):
        return self.dc_type == self.DCType.DEC
    
    def get_inc_value(self):
        return self.inc_value

    def get_dec_value(self):
        return self.dec_value


class DirectedGraph:
    def __init__(self, vertices_num = 1, edges = []) -> None:
        self.vertices_num = vertices_num
        self.edges = edges
        # default dictionary to store graph
        self.graph = defaultdict(list)
        self.time = 0
        self.scc_ids = [-1] * (self.vertices_num)
        self.scc_cnt = -1
        pass

    def build_edges(self):
        for (u, v) in self.edges:
            self.graph[u].append(v)

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

    



class LocalBound:
    transition_local_bounds = {}
    #
    def __init__(self, transition_graph = TransitionGraph()):
        self.transition_graph = transition_graph
    
    @staticmethod
    def compute_local_bounds(transition_graph):
        transition_local_bounds = ["-1"]*len(transition_graph.transitions)
        for index, (_, dc_set, _, _) in enumerate((transition_graph.transitions)):
            # (_, dc_set, _, _) = transition_graph.transitions[transition_index]
            if not transition_graph.is_in_scc(transition_graph.edges[index]):
                transition_local_bounds[index] = "1"
            else:
                for dc in dc_set:
                    if dc.is_dec():
                        transition_local_bounds[index] = dc.get_var()                
        
        for index, local_bound in enumerate(transition_local_bounds):
            # (_, dc_set, _, _) = transition_graph.transitions[transition_index]
            if local_bound == "-1":
                for i_other, lb_other in enumerate(transition_local_bounds):
                    if lb_other != "-1" and (not DirectedGraph(transition_graph.vertices_num, transition_graph.edges[:i_other]+transition_graph.edges[i_other+1:]).is_in_scc(transition_graph.edges[index])):
                        transition_local_bounds[index] = lb_other
                        continue
        return transition_local_bounds






class TransitionBound:
    transition_bounds = []
    var_invariant = {}
    transition_local_bounds = []
    
    def __init__(self, transition_graph = TransitionGraph()) -> None:
        self.transition_graph = transition_graph
        self.transition_bounds = [""]*len(transition_graph.transitions)
        self.transition_local_bounds = LocalBound.compute_local_bounds(transition_graph)
        print (self.transition_local_bounds)
        self.var_invariant = defaultdict(str)
        self.var_incs = defaultdict(list)
        self.var_incs_bound = defaultdict(str)
        self.var_resets = defaultdict(list)
        
    def compute_var_inc_and_reset(self):
        # self.var_incs = {}
        for transition_index in range(len(self.transition_graph.transitions)):
            (_, dc_set, _, _) = self.transition_graph.transitions[transition_index]
            # (_, dc_set, _, _) = t
            for dc in dc_set:
                if dc.dc_type == DifferenceConstraint.DCType.INC:
                    self.var_incs[dc.get_var()].append((transition_index, dc.dc_const))
                elif dc.dc_type == DifferenceConstraint.DCType.RESET:
                    self.var_resets[dc.get_var()].append((transition_index, dc.dc_var, dc.dc_const))
    
    # def compute_var_reset(self):
    #     self.var_resets = {}
    
    # Input: a variable
    # computes the symbolic invariant for this variable over the whole program
    # Save this result into the global storage : self.var_invariant
    # to avoid re-computation
    def compute_var_invariant(self, v):
        var_inc = "0"
        var_reset = "0"
        # print(v, self.var_resets[v], self.var_incs[v])
        for (t, dc_const) in self.var_incs[v]:
            var_inc += " + " + self.transition_bounds[t] + " * " + dc_const
        for (t, dc_var, dc_const) in self.var_resets[v]:
            var_reset = "max(" + var_reset + ", " + (self.var_invariant[dc_var] if dc_var else "0") + " + " + dc_const + ")"

        self.var_incs_bound[v] = var_inc
        self.var_invariant[v] = var_inc + " + " + var_reset



    def compute_transition_bound_closure(self, t_index):
        if not self.transition_bounds[t_index] == "":
            return self.transition_bounds[t_index]
        v = self.transition_local_bounds[t_index]
        if v == "1":
            self.transition_bounds[t_index] = "1"
            return "1"
        if v == "Q":
            self.transition_bounds[t_index] = "max(DB)"
            return "1"
        if v == "-1":
            self.transition_bounds[t_index] = "INF"
            return "INF"
        else:
            if v not in self.var_invariant.keys():
                self.compute_var_invariant(v)
            tb_temp = self.var_incs_bound[v]
            for (reset_t, dc_var, dc_const) in self.var_resets[v]:
                if self.transition_bounds[reset_t] == "":
                    self.compute_transition_bound_closure(reset_t)
                if not dc_var:
                    tb_temp += " + " + self.transition_bounds[reset_t] + " * "  + dc_const              
                else:
                    if dc_var not in self.var_invariant.keys():
                        self.compute_var_invariant(dc_var)
                    tb_temp += " + " + self.transition_bounds[reset_t] + " * (" +  self.var_invariant[dc_var] + " + " + dc_const + ")"              
            self.transition_bounds[t_index] = tb_temp
    
    def compute_transition_bounds(self):
        self.compute_var_inc_and_reset()
        for transition_index in range(len(self.transition_graph.transitions)):
            self.compute_transition_bound_closure(transition_index)
        return self.transition_bounds


class VariableReachingBound:
    def __init__(self, graph=Graph(), transition_graph=TransitionGraph()) -> None:
        self.graph = graph
        self.transition_graph = transition_graph
    
    def attach_weights(self):
        transition_bounds = TransitionBound(self.transition_graph).compute_transition_bounds()
        for (t_index, b) in enumerate(transition_bounds):
            transition = self.transition_graph.transitions[t_index]
            for var_vertex in transition[3] :
                self.graph.weights[var_vertex] = self.graph.weights[var_vertex] + AdaptType(b)
    
    def get_weights(self):
        return [w.value for w in self.graph.weights]
    
    def print_weights(self):
        for transition in self.transition_graph.transitions:
            for var_vertex in transition[3]:
                if not transition[1] == []:
                #     print( "weight for a testing command of label : " + str(var_vertex) + " is: " + str(self.graph.weights[var_vertex].value))
                # else:
                    print( "weight for Variable: " + transition[1][0].get_var() + " of label " + str(var_vertex) + " is: " + str(self.graph.weights[var_vertex].value))


