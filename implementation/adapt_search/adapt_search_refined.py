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

    def create_edges (self):
        for e in self.graph.edges:
            self.edges.append(self.Edge(e[1], self.head[e[0]]))
            # print(self.edges[-1].to, self.edges[-1].next)
            self.head[e[0]] = len(self.edges) - 1
            # print(self.head)

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
                # self.scc_adapt[self.scc_cnt] = (self.scc_adapt[self.scc_cnt] + self.graph.weights[x]) if self.graph.query[x] else self.scc_adapt[self.scc_cnt]
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
            if not self.visited[v]:
                self.visited[v] = True
                self.refined_adapt_calculation_dfs(v)
                # print("in the visiting #: ", self.dfs_clock, ". after visit vertex ", v, "the first visit is: ", self.first_visit[v], 
                #     "the last visit is: ", self.last_visit[v])
            else:
                self.adapt[v] = self.adapt[v].adapt_max(self.flow_capacity[v] * self.query_num[v])
                self.flow_capacity[v] = self.graph.weights[v]
                self.query_num[v] = self.graph.query[v]
                # print("in the visiting #: ", self.dfs_clock, "the visited vertex ", v, ", whoes first visit is: ", self.first_visit[v], 
                #     "the last visit is: ", self.last_visit[v])
            i = self.edges[i].next
        


    def bfs_adapt(self):
        bfs_q = []
        self.visited = [False]*(self.vertex_no + 1)
        self.adapt = [AdaptType(0)]* (self.scc_cnt+2)
        # for i in range(1, self.scc_cnt+1):
        #     self.adapt[i] = self.scc_adapt[i] if self.scc_no[0] == i  else AdaptType(0)
        bfs_q.append(self.scc_no[0])
        while bfs_q != []:
            u = bfs_q.pop(0)
            # print(u)
            # self.visited[u] = False
            self.refined_adapt_calculation_dfs(u)
            for v in self.scc_graph[u]:
                # self.adapt[v] = (self.adapt[v].adapt_max(self.adapt[u] + self.scc_adapt[v]))
                if not self.visited[v]:
                    self.visited[v] = True 
                    bfs_q.append(v)

    def get_adapt(self):
        adaptivity = AdaptType(0)
        for adapt_value in self.adapt:
            adaptivity = adaptivity.adapt_max(adapt_value)
        return adaptivity.value
    
    def search_adapt(self):
        self.create_edges()
        self.find_scc()
        self.build_scc()
        self.bfs_adapt()

