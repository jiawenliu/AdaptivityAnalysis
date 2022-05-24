from adapt_search_naive import AdaptSearchAlg, AdaptType, Graph


class AdaptSearchAlgRefined(AdaptSearchAlg):
    class Edge:
        to = -1
        next = -1

        def __init__(self, to = -1, next = -1):
            self.to = to 
            self.next = next        

    def __init__(self, graph = Graph()):
        AdaptSearchAlg.__init__(self, graph)
        self.flow_capacity = [AdaptType(0)] * (self.vertex_no + 1)
        self.query_num = [0] * (self.vertex_no + 1)
        self.refined_adapt = [AdaptType(1) if self.graph.query[v] else AdaptType(0) for v in range(self.vertex_no)]
        self.refined_adapt_visited = [False] * (self.vertex_no + 1)

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
            # print("the vertex #: ", v, "is assigned a SCC number or not", (not self.scc_no[v]))
            if not self.first_visit[v]:
                self.two_direction_dfs(v)
                self.last_visit[u] = min(self.last_visit[u], self.last_visit[v])
                # print("in the visiting #: ", self.dfs_clock, ". after visit vertex ", v, "the first visit is: ", self.first_visit[v], 
                #     "the last visit is: ", self.last_visit[v])
            elif not self.scc_no[v]:
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
                self.scc_no[x] = self.scc_cnt
                self.refined_adapt_visited[x] = True
                self.refined_adapt_calculation_dfs(x)
                self.scc_adapt[self.scc_cnt] = self.scc_adapt[self.scc_cnt].adapt_max(self.refined_adapt[x])
                # print(self.scc_adapt)
                if x == u:
                    break
            # print("new scc # : ", self.scc_cnt, "with vetices: ", self.scc_stack)
            # print(self.scc_no)



    def refined_adapt_calculation_dfs (self, u: int):
        i = self.head[u]
        while i != -1:
            v = self.edges[i].to
            self.flow_capacity[v] = self.flow_capacity[u].adapt_min(self.graph.weights[v])
            self.query_num[v] = self.query_num[u] + self.graph.query[v]
            self.refined_adapt[v] = self.refined_adapt[u]
            if not self.refined_adapt_visited[v]:
                self.refined_adapt_visited[v] = True
                self.refined_adapt_calculation_dfs(v)
                # print("in the visiting #: ", self.dfs_clock, ". after visit vertex ", v, "the first visit is: ", self.first_visit[v], 
                #     "the last visit is: ", self.last_visit[v])
            else:
                self.refined_adapt[v] = self.refined_adapt[v].adapt_max(self.flow_capacity[v] * AdaptType(self.query_num[v]))
                self.flow_capacity[v] = self.graph.weights[v]
                self.query_num[v] = self.graph.query[v]
                # print("in the visiting #: ", self.dfs_clock, "the visited vertex ", v, ", whoes first visit is: ", self.first_visit[v], 
                #     "the last visit is: ", self.last_visit[v])
            i = self.edges[i].next
        



