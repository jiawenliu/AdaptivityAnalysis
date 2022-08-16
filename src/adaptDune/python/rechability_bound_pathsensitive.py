from collections import defaultdict
from abstract_transition_graph import TransitionGraph, DifferenceConstraint
from bound_infer import TransitionBound
from program_refine import RefinedProg


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
        #### TODO: ADD the restriction on the label order.
        ### THE label should smaller than first label of prog
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
        #### TODO: ADD the restriction on the label order.
        ### THE label should be contained in the prog

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
        #### TODO: ADD the restriction on the label order.
        ### THE label should smaller than first label of tp_prog and greater than loop_prog
        initial = self.prog_initial(tp_prog)
        final = " ¬(" + self.prog_invariant(tp_prog) + ")"
        next = self.prog_next(loop_prog)
        return "(" + initial + "->"  + final + ")/(" + initial + "-" + next + ")"


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
        for  transition_path, bound in self.transition_path_ps_bound.items():
            print("path Sensitive Reachability Bound for the Transition Path : ", transition_path, " is : ", bound)


