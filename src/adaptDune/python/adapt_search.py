from adapt_lib import AdaptType, Graph
import math



class AdaptSearchAlg:
    class Edge:
        to = -1
        next = -1

        def __init__(self, to = -1, next = -1):
            self.to = to 
            self.next = next        


    def __init__(self, graph = Graph()):
        self.graph = graph
        self.vertex_no =graph.get_vertice_num()
        self.edges = []
        self.first_visit = [0]*self.vertex_no
        self.last_visit = [0]*self.vertex_no
        
        self.scc_adapt = [AdaptType(0)]* (self.vertex_no + 1)
        self.head = [-1]*self.vertex_no
        self.scc_stack = []
        self.scc_graph = [[]]*(self.vertex_no+1)
        self.scc_id = [0]*self.vertex_no
        self.scc_cnt = 0
        self.dfs_clock = 0
        self.adaptivity = AdaptType(0)
        

    def create_edges (self):
        for e in self.graph.edges:
            self.edges.append(self.Edge(e[1], self.head[e[0]]))
            self.head[e[0]] = len(self.edges) - 1
            # print(self.head)


    def find_scc(self):
        self.scc_id = [0]*self.vertex_no
        self.first_visit = [0]*self.vertex_no
        self.scc_adapt = [AdaptType(0)]*(self.vertex_no + 1)
        self.scc_cnt = self.dfs_clock = 0
        for i in range(0, self.vertex_no):
            if not self.first_visit[i]:
                self.two_direction_dfs(i)
        # print("The SCC adaptivity: ", list(map(lambda a: a.value, self.scc_adapt)))


    def build_scc(self):
        self.scc_graph = [set() for _ in range(self.vertex_no + 1)]
        for u in range(0, self.vertex_no):
            # print("vertex ", u, " is the head of edge # ", self.head[u], "to the node: ", self.edges[self.head[u]].to)
            i = self.head[u]
            # print("vertex ", u, "belongs to the scc # ", self.scc_id[u])
            while i != -1:
                v = self.edges[i].to
                if not self.scc_id[u] == self.scc_id[v]:
                    self.scc_graph[self.scc_id[u]].add(self.scc_id[v])
                i = self.edges[i].next
        # print("The SCC graph: ", self.scc_graph)

  

    def bfs_adapt(self):
        self.adapt = [AdaptType(0)]* (self.scc_cnt+2)
        if sum(self.graph.query) == 0:
            return
        visited = [False]*(self.vertex_no + 1)

        def bfs(start_scc, visited):

            bfs_q = [start_scc]
            while bfs_q != []:
                scc_v = bfs_q.pop(0)
                for scc_u in self.scc_graph[scc_v]:
                    # print("SCC {} is connected to the unvisited SCC {}".format(scc_v, scc_u))
                    if not visited[scc_u]:
                        visited[scc_u] = True 
                        bfs_q.append(scc_u)
                    self.adapt[scc_u] = (self.adapt[scc_u].adapt_max(self.adapt[scc_v] + self.scc_adapt[scc_u]))


        # BFS For each SCC
        for v in range(self.vertex_no):
            scc_v = self.scc_id[v]
            if not visited[scc_v] and self.graph.query[v] == 1:
                visited[scc_v] = True
                self.adapt[scc_v] = self.scc_adapt[scc_v]
                bfs(scc_v, visited)
                # print("After bfs from the vertex : {}, the Visited Status Are: {}".format(v, visited))
                # print("the Updated Adaptivity Are: {}".format(self.adapt))

        # print("Adaptivity of each SCC: ", list(map(lambda a: a.value, self.adapt)) )

    def print_adapt(self):
        print("The Adaptivity From This Graph is: ", self.get_adapt())
        query_num = AdaptType(0)
        for qn in [(AdaptType(self.graph.query[i]) * self.graph.weights[i]) for i in range(self.graph.get_vertice_num())]:
            query_num += qn
        # print("The Total Query Number For This Graph is: ", query_num.value)
        # print("The Estimated Generalization Error with an Optimial qurey computation Mechanism is O(", 
        # (self.adaptivity  * AdaptType((math.sqrt(sum(self.graph.query))))).value, "/"+f"√N )")


    def get_adapt(self):
        return self.adaptivity.value
    
    def search_adapt(self):
        self.create_edges()
        self.find_scc()
        self.build_scc()
        self.bfs_adapt()
        for adapt_value in self.adapt:
            self.adaptivity = self.adaptivity.adapt_max(adapt_value)

    def comptue_total_query_num(self):
        total_query = AdaptType(0)
        for v in range(self.vertex_no):
            total_query += self.graph.weights[v] if self.graph.query[v] else AdaptType(0)
        return total_query

    def print_query_num(self):
        total_query = self.comptue_total_query_num()
        print("The Total Query Number For This Graph is: ", total_query.value)
        print("The Estimated Generalization Error with an Optimial qurey computation Mechanism is O({} * √log({})/√N )".format(self.get_adapt(),total_query.value))


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
            scc_iddes = []
            while True:
                x = self.scc_stack.pop()
                self.scc_id[x] = self.scc_cnt
                scc_iddes.append(x)
                if x == u:
                    break
            # print("new scc # : ", self.scc_cnt, "with vetices: ", scc_iddes)

            self.refined_adapt_visited[min(scc_iddes)] = True
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
            # print("visiting vertex", v, "from vertex", u, " with its query annotation: ",  self.graph.query[v],
            # "and accumulated query number ",  self.query_num[u])
            if not self.refined_adapt_visited[v]:
                self.refined_adapt_visited[v] = True
                self.refined_adapt_calculation_dfs(v)
                # print("in the visiting #: ", self.dfs_clock, ". after visit vertex ", v, "the first visit is: ", self.first_visit[v], 
                #     "the last visit is: ", self.last_visit[v])
            else:
                if v == u:
                    # print("update the adaptivity with self loop  with accumulate adaptivity", self.refined_adapt[v].value,
                    # "with weight ", self.graph.weights[v].value, " and accumulate weight and query number", self.flow_capacity[v].value, self.query_num[v])
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
                # print("in the visiting #: ", self.dfs_clock, "the visited vertex ", v, ", whoes first visit is: ", self.first_visit[v], 
                #     "the last visit is: ", self.last_visit[v])
            i = self.edges[i].next
       



