from asyncore import loop
import enum
from collections import defaultdict
from functools import reduce
from ntpath import join
from bound_infer import DifferenceConstraint, TransitionBound, TransitionGraph

# class DifferenceConstraint:
#     class DCType(enum.Enum):
#         DEC = 1
#         INC = 2
#         RESET = 3

#     var = ""
#     dc_var = ""
#     dc_const = ""

#     dc_type = 1
#     def __init__(self, var="x", dc_var = None, dc_const = None, dc_type = 1) -> None:
#         self.var = var 
#         self.dc_var = dc_var
#         self.dc_const = dc_const
#         self.dc_type = dc_type
#         pass

#     def get_var(self):
#         return self.var 

#     def is_reset(self):
#         return self.dc_type == 3
    
#     def is_inc(self):
#         return self.dc_type == 2

#     def is_dec(self):
#         return self.dc_type == self.DCType.DEC
    



# class DirectedGraph:
#     def __init__(self, vertices_num = 1, edges = []) -> None:
#         self.vertices_num = vertices_num
#         self.edges = edges
#         # default dictionary to store graph
#         self.graph = defaultdict(list)
#         self.time = 0
#         self.scc_ids = [-1] * (self.vertices_num)
#         self.scc_cnt = -1
#         pass

#     def build_edges(self):
#         for (u, v) in self.edges:
#             self.graph[u].append(v)

#     def is_in_scc(self, edge):
#         if self.scc_cnt == -1:
#             self.build_edges()
#             self.build_scc()
#         (u, v) = edge
#         return self.scc_ids[u] == self.scc_ids[v]
    
#     def scc_dfs(self,u, low, disc, stackMember, st):

#         # Initialize discovery time and low value
#         disc[u] = self.time
#         low[u] = self.time
#         self.time += 1
#         stackMember[u] = True
#         st.append(u)

#         # Go through all vertices adjacent to this
#         for v in self.graph[u]:
            
#             # If v is not visited yet, then recur for it
#             if disc[v] == -1 :
            
#                 self.scc_dfs(v, low, disc, stackMember, st)

#                 # Check if the subtree rooted with v has a connection to
#                 # one of the ancestors of u
#                 # Case 1 (per above discussion on Disc and Low value)
#                 low[u] = min(low[u], low[v])
                        
#             elif stackMember[v] == True:

#                 '''Update low value of 'u' only if 'v' is still in stack
#                 (i.e. it's a back edge, not cross edge).
#                 Case 2 (per above discussion on Disc and Low value) '''
#                 low[u] = min(low[u], disc[v])

#         # head node found, pop the stack and print an SCC
#         w = -1 #To store stack extracted vertices
#         if low[u] == disc[u]:
#             self.scc_cnt += 1
#             while w != u:
#                 w = st.pop()
#                 # print (w, end=" ")
#                 self.scc_ids[w] = self.scc_cnt
#                 stackMember[w] = False
                
#             # print()
   

#     #The function to do DFS traversal.
#     # It uses recursive scc_dfs()
#     def build_scc(self):

#         # Mark all the vertices as not visited
#         # and Initialize parent and visited,
#         # and ap(articulation point) arrays
#         disc = [-1] * (self.vertices_num)
#         low = [-1] * (self.vertices_num)
#         stackMember = [False] * (self.vertices_num)
#         st =[]
        

#         # Call the recursive helper function
#         # to find articulation points
#         # in DFS tree rooted with vertex 'i'
#         for i in range(self.vertices_num):
#             if disc[i] == -1:
#                 self.scc_dfs(i, low, disc, stackMember, st)   


# #Inherit the Transition Graph and the Data-Flow Graph from the Command Graph Class
# class TransitionGraph(DirectedGraph):
#     ctl_edges = [(0, 1), (1, 1)]
#     transitions = [(0, [DifferenceConstraint("x", None, "k", DifferenceConstraint.DCType.RESET)], 1, [0]),
#     (1, [DifferenceConstraint("x", None, "1", DifferenceConstraint.DCType.DEC)], 1, [1, 2])]

#     def __init__(self, 
#     edges=[(0, 1), (1, 1)], 
#     transitions=[(0, [DifferenceConstraint("x", None, "k", DifferenceConstraint.DCType.RESET)], 1, [0]),
#     (1, [DifferenceConstraint("x", None, "1", DifferenceConstraint.DCType.DEC)], 1, [1, 2])],
#     vertex_num = None
#     ):

#         super().__init__(vertex_num if vertex_num else (max(map(lambda x: max(x), edges)) + 1), edges)
#         self.ctl_edges = edges
#         self.transitions = transitions

    



# class LocalBound:
#     transition_local_bounds = {}
#     #
#     def __init__(self, transition_graph = TransitionGraph()):
#         self.transition_graph = transition_graph
    
#     @staticmethod
#     def compute_local_bounds(transition_graph):
#         transition_local_bounds = [("-1", 1)]*len(transition_graph.transitions)
#         for index, (_, dc_set, _, _) in enumerate((transition_graph.transitions)):
#             # (_, dc_set, _, _) = transition_graph.transitions[transition_index]
#             if not transition_graph.is_in_scc(transition_graph.edges[index]):
#                 transition_local_bounds[index] = ("1", 1)
#             else:
#                 for dc in dc_set:
#                     if dc.is_dec():
#                         transition_local_bounds[index] = (dc.get_var(), (dc.dc_const))                
        
#         for index, (local_bound, lb_c) in enumerate(transition_local_bounds):
#             # (_, dc_set, _, _) = transition_graph.transitions[transition_index]
#             if local_bound == "-1":
#                 for i_other, (lb_other, lb_c_other) in enumerate(transition_local_bounds):
#                     if lb_other != "-1" and (not DirectedGraph(transition_graph.vertices_num, transition_graph.edges[:i_other]+transition_graph.edges[i_other+1:]).is_in_scc(transition_graph.edges[index])):
#                         transition_local_bounds[index] = (lb_other, lb_c_other)
#                         continue
#         return transition_local_bounds




class ProgramRefine():

    def __init__(self) -> None:
        self.refined_result = []
        pass

    def collect_paths(self):
        pass

    def program_refine(self):
        pass 

    def get_result(self):
        return self.refined_result

class RefinedProg():
    class RType(enum.Enum):
        CHOICE = 1
        REPEAT = 2
        SEQ = 3
        TP = 4
    
    # type: The type of the refined program
    # prog: List of Refined Program
    def __init__(self, type = None, prog = None, loop_label = None):
        self.type = type
        self.prog = prog
        self.loop_label = loop_label

    def get_loop_label(self):
        return self.loop_label

    def get_choices(self):

        return self.prog

    def get_seqs(self):
        return self.prog

    def get_repeat(self):
        return self.prog
    
    def get_tp(self):
        return self.prog
    
    # # dfs until the TP, return the list of assumptions on all the transition paths.
    # def get_assumes(self):
    #     return ["True"]
    
    def get_transitions(self):
        if self.type == RefinedProg.RType.CHOICE:
            return reduce(lambda a, b: a + b, (choice_p.get_transitions() for choice_p in self.get_choices()), [])
        elif self.type == RefinedProg.RType.REPEAT:
            return self.get_repeat().get_transitions()
        elif self.type == RefinedProg.RType.SEQ:
            return reduce(lambda a, b: a + b, (seq_prog.get_transitions() for seq_prog in self.get_seqs()), [])
        elif self.type == RefinedProg.RType.TP:
            return (self.prog)

    def prog_id(self):
        t = self.get_transitions()
        return str(t)


    def prog_signature(self):
        if self.type == RefinedProg.RType.CHOICE:
            return "CH : {" + ",".join(choice_p.prog_signature() for choice_p in self.get_choices()) + "}"
        elif self.type == RefinedProg.RType.REPEAT:
            return "RP : (" + self.get_repeat().prog_signature() + ")"
        elif self.type == RefinedProg.RType.SEQ:
            return "SEQ : (" + ",".join(seq_prog.prog_signature() for seq_prog in self.get_seqs()) + ")"
        elif self.type == RefinedProg.RType.TP:
            return str(self.prog)



class PathSensitiveReachabilityBound():

    def __init__(self, transition_graph=TransitionGraph()) -> None:
        self.transition_graph = transition_graph
        self.transition_paths = []
        self.tp_var_gd = []
        self.prog_loc_bound =  defaultdict(str)
        self.transition_path_rpchain_bound =  defaultdict(str)
        self.prog_bound =  defaultdict(str)
        self.transition_bound = TransitionBound(transition_graph)
        self.transition_path_ps_bound =  defaultdict(str)
        self.loop_chains = defaultdict(list)
 
    # def transition_bound(self, transition_graph):
    #     return TransitionBound(transition_graph).compute_bounds()

    # def program_refine(self):
    #     self.refined_prog = ProgramRefine(self.transition_graph).get_result() 

    def outside_in(self, refined_prog):
        if refined_prog.type == RefinedProg.RType.CHOICE:
            return ("max(" + ",".join(self.outside_in(choice_prog) for choice_prog in refined_prog.get_choices()) + ")")
        elif refined_prog.type == RefinedProg.RType.REPEAT:
            rp_prog = refined_prog.get_repeat()
            return "(" + self.prog_initial(rp_prog) + " until "  + self.prog_final(rp_prog) + ") / (" + self.var_gd(rp_prog)
        elif refined_prog.type == RefinedProg.RType.SEQ:
            return ("(" + "+".join(self.outside_in(seq_prog) for seq_prog in refined_prog.get_seqs()) + ")")
        elif refined_prog.type == RefinedProg.RType.TP:
            return "1"

    def var_gd(self, prog):
        id = prog.prog_id()
        if (not self.prog_loc_bound[id]):
           self.prog_loc_bound[id] = self.outside_in(prog)
        ## For Debuging:
        next = self.prog_next(prog)
        print("the NEXT state for program : ", prog.prog_id(), " is : ", next)

        ## For Releasing:
        return self.prog_loc_bound[id]  + " * ("  +  self.prog_initial(prog) + " - "  + self.prog_next(prog) + ")"

    # def transition_path_next(self):
    #     return [self.var_incs[v] - self.var_decs[v] for v in self.vars]

    # dfs until the TP, return the list of unique variables on all the transition paths.
    def get_vars(self, prog):
        transitions = prog.get_transitions()
        r = set()
        for (_, dc_set, _, _) in [self.transition_graph.transitions[t_id] for t_id in transitions]:
            for dc in dc_set:
                if(not (dc.dc_type == DifferenceConstraint.DCType.ASUM)):
                    (r.add(dc.var))
        ## For Debuging:
        print("The Variables Set for Program : ", prog.prog_id(), " is : ", r)
        return r

    # dfs until the TP, return the list of assumptions on all the transition paths.
    def get_assumes(self, prog):
        transitions = prog.get_transitions()
        r = set()
        for (_, dc_set, _, _) in [self.transition_graph.transitions[t_id] for t_id in transitions]:
            for dc in dc_set:
                if dc.dc_type == DifferenceConstraint.DCType.ASUM:
                    (r.add(dc.dc_bexpr))
        ## For Debuging:
        print("The Assumptions for Program : ", prog.prog_id(), " is : ", r)
        return r


    def prog_initial(self, prog):
        ## For Debuging:
        temp = "/\\".join([str(v) + "= " + str(self.transition_bound.var_invariant[v]) for v in self.get_vars(prog)])
        print("the INITIAL state for program : ", prog.prog_id(), " is : ", temp)
        ## For Releasing:
        return "/\\".join([str(v) + " = " + str(self.transition_bound.var_invariant[v]) for v in self.get_vars(prog)])

    def prog_final(self, prog):
        f = self.prog_invariant(prog)
        print("the FINAL state for program : ", prog.prog_id(), " is : ", "¬(" + f + ")")

        return " ¬(" + self.prog_invariant(prog) + ")"

    def prog_next(self, prog):
        if prog.type == RefinedProg.RType.CHOICE:
            return ("max(" + ",".join(self.prog_next(choice_p) for choice_p in prog.get_choices()) + ")")
        elif prog.type == RefinedProg.RType.REPEAT:
            rp_prog = prog.get_repeat()
            rp_id = rp_prog.prog_id()
            if (not self.prog_loc_bound[rp_id]):
                self.prog_loc_bound[rp_id] = self.outside_in(rp_prog)
            return self.prog_loc_bound[rp_id]  + " * ("  + (self.prog_next(rp_prog)) + ")"
        elif prog.type == RefinedProg.RType.SEQ:
            return ("(" + "+".join(self.prog_next(seq_prog) for seq_prog in prog.get_seqs()) + ")")
        elif prog.type == RefinedProg.RType.TP:
            return ("(" + "+".join([
                (str("+".join([inc[1] for inc in self.transition_bound.var_incs[v]])) if self.transition_bound.var_incs[v] else "0") for v in self.get_vars(prog)]) + ")")

    def prog_invariant(self, prog):
        return "/\\".join(self.get_assumes(prog))



    def inside_out(self, prog):
        self.repeat_chain_dfs(prog, "1")
        self.loop_chain_dfs(prog, [])
        self.compute_transition_path_ps_bound(prog)

    def repeat_chain_dfs(self, prog, rp_bound, L=None):
        print("Computing the Repeat Chain for prog : ", prog.prog_signature())
        L = prog.get_loop_label() if prog.get_loop_label() else L
        rp_bound = "1" if prog.get_loop_label() else rp_bound
        if prog.type == RefinedProg.RType.CHOICE:
            for choice_prog in prog.get_choices():
                (self.repeat_chain_dfs(choice_prog, rp_bound, L))
        elif prog.type == RefinedProg.RType.REPEAT:
            self.repeat_chain_dfs(prog.get_repeat(), self.prog_loc_bound[prog.prog_id()]  + " * ("  + rp_bound + ")", L)
        elif prog.type == RefinedProg.RType.SEQ:
            for seq_prog in prog.get_seqs():
                (self.repeat_chain_dfs(seq_prog, rp_bound, L))
        elif prog.type == RefinedProg.RType.TP:
            self.transition_path_rpchain_bound[prog.prog_id()] = (L, rp_bound)
        else:
            return

    
    def loop_chain_dfs(self, prog, loop_chain):
        print("Computing the Loop Chain for prog : ", prog.prog_signature())
        tmp = loop_chain + [(prog.get_loop_label(), prog)] if prog.get_loop_label() else loop_chain
        if prog.type == RefinedProg.RType.CHOICE:
            for choice_prog in prog.get_choices():
                (self.loop_chain_dfs(choice_prog, tmp))
        elif prog.type == RefinedProg.RType.REPEAT:
            self.loop_chain_dfs(prog.get_repeat(), tmp)
        elif prog.type == RefinedProg.RType.SEQ:
            for seq_prog in prog.get_seqs():
                (self.loop_chain_dfs(seq_prog, loop_chain))
        elif prog.type == RefinedProg.RType.TP:
            return self.loop_chains[prog.prog_id()].append(loop_chain)
        else:
            return
    
    def compute_loop_chain_bound(self, tp_prog, loop_chain):
        ### FOR DEBUGGING
        if not (loop_chain): return "1"
        (_, tp_rpchain_bound) = self.transition_path_rpchain_bound[tp_prog.prog_id()]
        loop_chain_bound = tp_rpchain_bound
        (_, tp_loop_prog) = loop_chain[-1]
        for (_, nested_loop_prog) in loop_chain[:-1]:
            loop_chain_bound += (" * " + self.compute_nested_lpchain_bound(nested_loop_prog, tp_loop_prog))
        print("Loop Chain Bound for the Transition Path : ", tp_prog.prog_id(), " is : ", loop_chain_bound)
        return loop_chain_bound
        ### FOR RELEASE
        if not (loop_chain): return "1"
        return self.transition_path_rpchain_bound[tp_prog.prog_id()][1] + ("*".join([self.compute_nested_lpchain_bound(loop_prog, loop_chain[-1][1]) for (_, loop_prog) in  loop_chain[:-1]]))

    def compute_nested_lpchain_bound(self, tp_prog, loop_prog):
        initial = self.prog_initial(tp_prog)
        final = " ¬(" + self.prog_invariant(tp_prog) + ")"
        next = self.prog_next(loop_prog)
        return "(" + initial + " -> "  + final + ")/(" + initial + "-" + next + ")"


    def compute_transition_path_ps_bound(self, prog):
        if prog.type == RefinedProg.RType.TP:
            self.transition_path_ps_bound[prog.prog_id()] = "max(" + ",".join(self.compute_loop_chain_bound(prog, loop_chain) for loop_chain in self.loop_chains[prog.prog_id()]) + ")"
        if prog.type == RefinedProg.RType.CHOICE:
            for choice_prog in prog.get_choices():
                (self.compute_transition_path_ps_bound(choice_prog))
        elif prog.type == RefinedProg.RType.REPEAT:
            self.compute_transition_path_ps_bound(prog.get_repeat())
        elif prog.type == RefinedProg.RType.SEQ:
            for seq_prog in prog.get_seqs():
                (self.compute_transition_path_ps_bound(seq_prog))
        else:
            return
    
    def compute_prog_bound(self, prog):
        p_id = (prog.prog_id())
        if prog.type == RefinedProg.RType.CHOICE:
            self.prog_bound[p_id] = max(self.compute_prog_bound(choice_prog) for choice_prog in prog.get_choices())
        elif prog.type == RefinedProg.RType.REPEAT:
            self.prog_bound[p_id] = self.prog_loc_bound[p_id] * self.compute_prog_bound(prog.get_repeat())
        elif prog.type == RefinedProg.RType.SEQ:
            self.prog_bound[p_id] = sum(self.compute_prog_bound(seq_prog) for seq_prog in prog.get_seqs())
        elif prog.type == RefinedProg.RType.TP:
            self.prog_bound[p_id] = 1
        else:
            return

    def compute_rb(self, prog):
        self.transition_bound.compute_transition_bounds()
        self.outside_in(prog)
        self.inside_out(prog)


    def print_path_bound(self):
        print("Number of Repeat Chain Bounds Computed for the Transition Path is : ", len(self.transition_path_rpchain_bound))
        for  transition, bound in self.transition_path_rpchain_bound.items():
            print("Repeat Chain Bound for the Transition Path : ", transition, " is : ", bound)
    
    def print_prog_bound(self):
        for prog, bound in self.prog_bound.items():
            print("Bound for the Loop at : ", prog, " is : ", bound)


    def print_loop_chain(self):
        for prog, loop_chains in self.loop_chains.items():
            print("Loop Chains for the transition path at : ", prog, " are : ")
            for loop_ch in loop_chains:
                print("loop chain: ")
                for (loop_id, lprog) in loop_ch:
                    print(" -> : L-", loop_id, "prog: ", lprog.prog_id())

    def print_transition_path_ps_bound(self):
        print("Number of Bounds Computed for the Transition Path is : ", len(self.transition_path_ps_bound))
        for  transition, bound in self.transition_path_ps_bound.items():
            print("path Sensitive Reachability Bound for the Transition Path : ", transition, " is : ", bound)


