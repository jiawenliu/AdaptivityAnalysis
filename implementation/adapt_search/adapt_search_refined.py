from itertools import accumulate
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
            scc_iddes = []
            while True:
                x = self.scc_stack.pop()
                # print(x)
                self.scc_id[x] = self.scc_cnt
                scc_iddes.append(x)
                # self.refined_adapt_visited[x] = True
                # self.refined_adapt_calculation_dfs(x)
                # self.scc_adapt[self.scc_cnt] = self.scc_adapt[self.scc_cnt].adapt_max(self.refined_adapt[x])
                # print(self.scc_adapt)
                if x == u:
                    break
            # print("new scc # : ", self.scc_cnt, "with vetices: ", self.scc_stack)
            # print(self.scc_id)
            print(scc_iddes)

            # for x in scc_iddes:
            #     self.refined_adapt_visited[x] = True
            #     self.refined_adapt_calculation_dfs(x)
            #     self.scc_adapt[self.scc_cnt] = self.scc_adapt[self.scc_cnt].adapt_max(self.refined_adapt[x])

            self.refined_adapt_visited[min(scc_iddes)] = True
            # self.flow_capacity[min(scc_iddes)] = self.graph.weights[min(scc_iddes)]
            self.refined_adapt_calculation_dfs(min(scc_iddes))
            self.scc_adapt[self.scc_cnt] = self.refined_adapt[min(scc_iddes)]


    def refined_adapt_calculation_dfs (self, u: int):
        i = self.head[u]
        while i != -1:
            v = self.edges[i].to
            if not (self.scc_id[u] == self.scc_id[v]):
                i = self.edges[i].next
                continue 
            query_num_temp = self.query_num[v]
            flow_capacity_temp = self.flow_capacity[v]
            self.flow_capacity[v] = self.flow_capacity[u].adapt_min(self.graph.weights[v])
            self.query_num[v] = self.query_num[u] + self.graph.query[v]
            self.refined_adapt[v] = self.refined_adapt[u]
            print("visiting vertex", v, "from vertex", u, " with its query annotation: ",  self.graph.query[v],
            "and accumulated query number ",  self.query_num[u])
            if not self.refined_adapt_visited[v]:
                self.refined_adapt_visited[v] = True
                self.refined_adapt_calculation_dfs(v)
                # print("in the visiting #: ", self.dfs_clock, ". after visit vertex ", v, "the first visit is: ", self.first_visit[v], 
                #     "the last visit is: ", self.last_visit[v])
            else:
                if v == u:
                    print("update the adaptivity with self loop  with accumulate adaptivity", self.refined_adapt[v].value,
                    "with weight ", self.graph.weights[v].value, " and accumulate weight and query number", self.flow_capacity[v].value, self.query_num[v])
                    self.refined_adapt[v] = (self.graph.weights[v] * AdaptType(self.graph.query[v]) + self.refined_adapt[v]).adapt_max(self.flow_capacity[v] * AdaptType(self.query_num[v]))
                else:
                    self.refined_adapt[v] = self.refined_adapt[v].adapt_max(self.flow_capacity[v] * AdaptType(self.query_num[v]))
                if i == -1: 
                    self.flow_capacity[v] = self.graph.weights[v]
                    self.query_num[v] = 0
                else: 
                    self.flow_capacity[v] = self.graph.weights[v]
                    self.query_num[v] = self.query_num[u]
                self.flow_capacity[v] = flow_capacity_temp
                self.query_num[v] = query_num_temp
                # self.flow_capacity[v] = self.flow_capacity[u]
                # self.query_num[v] = self.query_num[u]
                # print("in the visiting #: ", self.dfs_clock, "the visited vertex ", v, ", whoes first visit is: ", self.first_visit[v], 
                #     "the last visit is: ", self.last_visit[v])
            i = self.edges[i].next
       



