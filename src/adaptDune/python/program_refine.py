        #TODO: implment the refined_prog
        # refined_prog = GraphParser("./examples/ps_reachability_bound/seq.br").parse_refined_prog()
        # 
import enum
from functools import reduce


class ProgramRefine():
    #TODO: 
    # Input: transition graph, 
    # Compute: refined Program with RefinedProg.RType

    def __init__(self, transition_graph) -> None:
        self.transition_graph = transition_graph
        self.refined_result = RefinedProg()
        pass


    def program_refine(self):
        # self.transition_graph.build_edges()
        self.transition_graph.transition_id_lookup()
        print(self.transition_graph.graph)
        def refine_dfs(curr_label, curr_transition_path, nested_while_path = []):
            print("CURRENT_LABEL: ", curr_label)
            print("CURRENT_Transition_Path: ", curr_transition_path)
            if len(self.transition_graph.graph[curr_label]) == 0:
                return  RefinedProg(RefinedProg.RType.TP, curr_transition_path)
            elif len(self.transition_graph.graph[curr_label]) == 1: 
                next_label = self.transition_graph.graph[curr_label][0]
                edge_id = str(curr_label) + "->" + str(next_label)
                next_transition_id = self.transition_graph.edge_indices[str(curr_label) + "->" + str(next_label)]
                print("Single Path to The Nest Label %d Trough Transition Edge: %s" % (next_label, edge_id))
                if self.transition_graph.transitions[curr_transition_path[0]][0] == next_label:
                    return RefinedProg(RefinedProg.RType.TP, curr_transition_path[max(0, len(nested_while_path)-1):] + [next_transition_id])
                else:
                    return refine_dfs(next_label, curr_transition_path+[next_transition_id], nested_while_path) 
            else:
                print("Building Mutiple Paths Through Multiple Next Labels:", self.transition_graph.graph[curr_label])
                [next_label_true, next_label_false] = self.transition_graph.graph[curr_label]
                next_transition_id_true, next_transition_id_false = self.transition_graph.edge_indices[str(curr_label) + "->" + str(next_label_true)], self.transition_graph.edge_indices[str(curr_label) + "->" + str(next_label_false)]
                if self.transition_graph.transitions[next_transition_id_true][1][0].transition_type == "WHILE":
                    ####################################### FOR DEBUG ###################################################
                    tprog1 = RefinedProg(RefinedProg.RType.TP, curr_transition_path)
                    print("Mutliple Path From While Header to The While Body via The Nest Label %d Through Transition: %d" % (next_label_true, next_transition_id_true))
                    body = refine_dfs(next_label_true, [next_transition_id_true], [])
                    print("Mutliple Path From While Header to The While Exit via The Nest Label %d Through Transition: %d" % (next_label_false, next_transition_id_false))
                    tprog2 = refine_dfs(next_label_false, curr_transition_path + [next_transition_id_false], nested_while_path + curr_transition_path + [next_transition_id_false])
                    return RefinedProg(RefinedProg.RType.SEQ, [tprog1, RefinedProg(RefinedProg.RType.REPEAT, body), tprog2])
                    ####################################### FOR RELEASE ###################################################
                    return RefinedProg(RefinedProg.RType.SEQ, [RefinedProg(RefinedProg.RType.TP, curr_transition_path), RefinedProg(RefinedProg.RType.REPEAT, body), refine_dfs(next_label_false, [next_transition_id_false])])
                else:
                    ####################################### FOR DEBUG ###################################################
                    print("Mutliple Path From If Header to The True Branch via The Nest Label %d Through Transition: %d" % (next_label_true, next_transition_id_true))
                    prog_true = refine_dfs(next_label_true, curr_transition_path + [next_transition_id_true], nested_while_path)
                    print("Mutliple Path From If Header to The False Branch via The Nest Label %d Through Transition: %d" % (next_label_true, next_transition_id_true))
                    prog_false = refine_dfs(next_label_false, curr_transition_path + [next_transition_id_false], nested_while_path)
                    return RefinedProg(RefinedProg.RType.CHOICE, [prog_true, prog_false])
                    ####################################### FOR RELEASE ###################################################
                    return RefinedProg(RefinedProg.RType.CHOICE, [refine_dfs(next_label_true, curr_transition_path + [next_transition_id_true]), refine_dfs(next_label_true, curr_transition_path + [next_transition_id_false])])
        return refine_dfs(1, [0], [])

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
