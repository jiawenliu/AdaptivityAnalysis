import enum
from collections import defaultdict

class AdaptType:
    value = 0
    def __init__(self, value = 0) -> None:
        self.value = value
    def __add__(self, other):
        if (isinstance(self.value, str)) or isinstance(other.value, str):
            return AdaptType(str(self.value) + " + " + str(other.value))
        else:
            # print(self.value, other.value, " both are int")
            return AdaptType(self.value + other.value)

    def __radd__(self, other):
        if (isinstance(self.value, str)) or isinstance(other.value, str):
            return AdaptType(str(other.value) + " + " + str(self.value))
        else:
            # print(self.value, other.value, " both are int")
            return AdaptType(self.value + other.value)
    
    def __mul__(self, other):
        if (isinstance(self.value, str)) or isinstance(other.value, str):
            return AdaptType(str(other.value) + " * " + str(self.value))
        else:
            # print(self.value, other.value, " both are int")
            return AdaptType(self.value * other.value)

    def adapt_max(self, other):
        if (isinstance(self.value, str)) and isinstance(other.value, str):
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

class TransitionGraph(Graph):
    locations = [0, 1]
    ctl_edges = [(0, 1), (1, 1)]
    transitions = [(0, [DifferenceConstraint("x", None, "k", DifferenceConstraint.DCType.RESET)], 1, [0]),
    (1, [DifferenceConstraint("x", None, "1", DifferenceConstraint.DCType.DEC)], 1, [1, 2])]

    def __init__(self, edges=[(0, 1), (2, 2)], weights=[AdaptType(1), AdaptType("k"), AdaptType("k")], query=[1, 0, 1]):
        super().__init__(edges, weights, query)
    
    def is_scc(self, t):
        scc_list = [False, True]
        return scc_list[t]



class LocalBound:
    transition_local_bounds = {}
    #
    def __init__(self, transition_graph = TransitionGraph()):
        self.transition_graph = transition_graph
    
    @staticmethod
    def compute_local_bounds(transition_graph):
        transition_local_bounds = ["0"]*len(transition_graph.transitions)
        for transition_index in range(len(transition_graph.transitions)):
            (_, dc_set, _, _) = transition_graph.transitions[transition_index]
            if not transition_graph.is_scc(transition_index):
                transition_local_bounds[transition_index] = "1"
            else:
                for dc in dc_set:
                    if dc.is_dec():
                        transition_local_bounds[transition_index] = dc.get_var()
        print(transition_local_bounds)
        return transition_local_bounds






class TransitionBound:
    transition_bounds = []
    var_invariant = {}
    transition_local_bounds = []
    
    def __init__(self, transition_graph = TransitionGraph()) -> None:
        self.transition_graph = transition_graph
        self.transition_bounds = [""]*len(transition_graph.transitions)
        self.transition_local_bounds = LocalBound.compute_local_bounds(transition_graph)
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
        print(v, self.var_resets[v], self.var_incs[v])
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


class VariableReachingBound:
    def __init__(self, graph, transition_graph) -> None:
        self.graph = graph
        self.transition_graph = transition_graph
    
    def attach_weights(self):
        transition_bounds = TransitionBound.compute_transition_bounds()
        for (t, b) in transition_bounds.items():
            for var_vertex in t[4]:
                self.graph.weights[var_vertex] = AdaptType(b)

transitionbound_algorithm = TransitionBound()
transitionbound_algorithm.compute_transition_bounds()
print(transitionbound_algorithm.transition_bounds)
