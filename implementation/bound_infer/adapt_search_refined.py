from adapt_search_naive import AdaptSearchAlg, AdaptType, Graph


class AdaptSearchAlgRefined(AdaptSearchAlg):       

    def __init__(self, graph = Graph()):
        AdaptSearchAlg.__init__(self, graph)
        self.flow_capacity = [self.graph.weights[v] for v in range(self.vertex_no)]
        self.query_num = [0] * (self.vertex_no + 1)
        self.refined_adapt = [AdaptType(1) if self.graph.query[v] else AdaptType(0) for v in range(self.vertex_no)]
        self.refined_adapt_visited = [False] * (self.vertex_no + 1)

    # @Override: two_direction_dfs
    # with a More Precise Adaptivity calculation Method: refined_adapt_calculation_dfs
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
            scc_nodes = []
            while True:
                x = self.scc_stack.pop()
                # print(x)
                self.scc_no[x] = self.scc_cnt
                scc_nodes.append(x)
                # self.refined_adapt_visited[x] = True
                # self.refined_adapt_calculation_dfs(x)
                # self.scc_adapt[self.scc_cnt] = self.scc_adapt[self.scc_cnt].adapt_max(self.refined_adapt[x])
                # print(self.scc_adapt)
                if x == u:
                    break
            # print("new scc # : ", self.scc_cnt, "with vetices: ", self.scc_stack)
            # print(self.scc_no)
            print(scc_nodes)

            # for x in scc_nodes:
            #     self.refined_adapt_visited[x] = True
            #     self.refined_adapt_calculation_dfs(x)
            #     self.scc_adapt[self.scc_cnt] = self.scc_adapt[self.scc_cnt].adapt_max(self.refined_adapt[x])

            self.refined_adapt_visited[min(scc_nodes)] = True
            # self.flow_capacity[min(scc_nodes)] = self.graph.weights[min(scc_nodes)]
            self.refined_adapt_calculation_dfs(min(scc_nodes))
            self.scc_adapt[self.scc_cnt] = self.refined_adapt[min(scc_nodes)]


    def refined_adapt_calculation_dfs (self, u: int):
        i = self.head[u]
        while i != -1 and self.scc_no[u] == self.scc_no[self.edges[i].to]:
            v = self.edges[i].to
            self.flow_capacity[v] = self.flow_capacity[u].adapt_min(self.graph.weights[v])
            query_num_temp = self.query_num[v]
            flow_capacity_temp = self.flow_capacity[v]
            self.query_num[v] = self.query_num[u] + self.graph.query[v]
            self.refined_adapt[v] = self.refined_adapt[u]
            i = self.edges[i].next
            print("visiting vertex", v)
            if not self.refined_adapt_visited[v]:
                self.refined_adapt_visited[v] = True
                self.refined_adapt_calculation_dfs(v)
                # print("in the visiting #: ", self.dfs_clock, ". after visit vertex ", v, "the first visit is: ", self.first_visit[v], 
                #     "the last visit is: ", self.last_visit[v])
            else:
                if v == u:
                    self.refined_adapt[v] = (self.graph.weights[v] + self.refined_adapt[v]).adapt_max(self.flow_capacity[v] * AdaptType(self.query_num[v]))
                else:
                    self.refined_adapt[v] = self.refined_adapt[v].adapt_max(self.flow_capacity[v] * AdaptType(self.query_num[v]))
                if i == -1: 
                    self.flow_capacity[v] = self.graph.weights[v]
                # self.flow_capacity[v] = AdaptType("MAX")
                    self.query_num[v] = 0
                else: 
                    self.flow_capacity[v] = self.graph.weights[v]
                # self.flow_capacity[v] = AdaptType("MAX")
                    self.query_num[v] = self.query_num[u]
                self.flow_capacity[v] = flow_capacity_temp
                # self.flow_capacity[v] = AdaptType("MAX")
                self.query_num[v] = query_num_temp
                # self.flow_capacity[v] = self.flow_capacity[u]
                # self.query_num[v] = self.query_num[u]
                # print("in the visiting #: ", self.dfs_clock, "the visited vertex ", v, ", whoes first visit is: ", self.first_visit[v], 
                #     "the last visit is: ", self.last_visit[v])
            # i = self.edges[i].next
        



