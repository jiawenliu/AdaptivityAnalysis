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

    def transition_path_collection(self):
        self.transition_graph.build_scc()
        pass

    def program_refine(self):
        # self.transition_graph.build_edges()
        self.transition_graph.transition_id_lookup()
        print(self.transition_graph.graph)
        def refine_dfs(curr_label, curr_transition_path):
            print("CURRENT_LABEL: ", curr_label)
            print("CURRENT_Transition_Path: ", curr_transition_path)
            # curr_label,curr_dc,next_label, _ = self.transition_graph.transitions[curr_transition_id]
            if len(self.transition_graph.graph[curr_label]) == 0:
                return  RefinedProg(RefinedProg.RType.TP, curr_transition_path)
            elif len(self.transition_graph.graph[curr_label]) == 1: 
                next_label = self.transition_graph.graph[curr_label][0]
                edge_id = str(curr_label) + "->" + str(next_label)
                next_transition_id = self.transition_graph.edge_indices[str(curr_label) + "->" + str(next_label)]
                print("Single Path to The Nest Label %d Trough Transition Edge: %s" % (next_label, edge_id))
                if self.transition_graph.transitions[curr_transition_path[0]][0] == next_label:
                    return RefinedProg(RefinedProg.RType.TP, curr_transition_path + [next_transition_id])
                else:
                    return refine_dfs(next_label, curr_transition_path+[next_transition_id]) 
            else:
                tprog1 = RefinedProg(RefinedProg.RType.TP, curr_transition_path)
                ch_progs = []
                print("Building Mutiple Paths Through Multiple Next Labels:", self.transition_graph.graph[curr_label])
                for next_label in self.transition_graph.graph[curr_label]:
                    edge_id = str(curr_label) + "->" + str(next_label)
                    next_transition_id = self.transition_graph.edge_indices[str(curr_label) + "->" + str(next_label)]
                    _,dc,_,var = self.transition_graph.transitions[next_transition_id]
                    print("Mutliple Path to One of The Nest Label %d Through Transition Edge: %s" % (next_label, edge_id))
                    if dc.transition_type == "WHILE":
                        ch_progs.append(refine_dfs(next_label, [self.transition_graph.transition_id[edge_id]]))
                    else:
                        ch_progs.append(refine_dfs(next_label, curr_transition_path + [next_transition_id]))
                return RefinedProg(RefinedProg.RType.SEQ, [tprog1, RefinedProg(RefinedProg.RType.CHOICE, ch_progs)])
            #TODO: need to mark the while ID
            # else:
                if self.transition_graph[curr_label].get_transition_type == 'while: id':
                    tprog1 = RefinedProg(RefinedProg.RType.TP, curr_transition_path)
                    ch_progs = []
                    for next_label in self.transition_graph.graph[curr_label]:
                        #TODO: check if it is going out of the same while: need to mark the while ID
                        if self.transition_graph[next_label].get_transition_type == 'while_out : id':
                            tprog2 = refine_dfs(next_label, [next_label])
                        else:
                            ch_progs.append(refine_dfs(next_label, [next_label]))
                    return RefinedProg(RefinedProg.RType.SEQ, [tprog1, RefinedProg(RefinedProg.RType.CHOICE, ch_progs), tprog2])
                elif curr_label == curr_transition_path[0]:
                    return RefinedProg(RefinedProg.RType.TP, curr_transition_path)
                else:
                    ch_progs = [refine_dfs(next_label, curr_transition_path+[next_label]) for next_label in self.transition_graph.graph[curr_label]]
                    if len(ch_progs) == 1: return ch_progs[0]
                    else: return RefinedProg(RefinedProg.RType.CHOICE, ch_progs)       
        return refine_dfs(1, [0])

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
