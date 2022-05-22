

class Edge:
    to = -1
    next = -1

    def __init__(self, to = -1, next = -1):
        self.to = to 
        self.next = next

weights = [1, 1, 2, 2]
query = [1, 1, 1, 1]
edges = [(0, 1), (1, 2), (2, 3), (3, 2), (2, 2)]

class AdaptSearchAlg:
        
    # vertex_no = 100000

    # edges = list[edge]

    # head = [-1]*10000
    # scc_cnt = 0
    # scc_no = [-1]*100000
    # dfs_clock = 0
    # first_visit = [-1]*10000
    # last_visit = [-1]*10000
    # adapt = [0]*100000
    def __init__(self, vertex_no = 100000):
        self.vertex_no =vertex_no
        self.edges = []
        self.first_visit = [0]*vertex_no
        self.last_visit = [0]*vertex_no
        self.adapt = [0]*vertex_no
        self.scc_adapt = [0]*vertex_no
        self.head = [-1]*vertex_no
        self.scc_stack = []
        self.scc_graph = [[]]*(vertex_no+1)
        self.scc_no = [0]*vertex_no
        self.scc_cnt = 0
        self.dfs_clock = 0

    def create_edge (self, e: int):
        self.edges.append(Edge(e[1], self.head[e[0]]))
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
                self.scc_adapt[self.scc_cnt] += weights[x] * query[x]
                # print(self.scc_adapt)
                if x == u:
                    break
            # print("new scc # : ", self.scc_cnt, "with vetices: ", self.scc_stack)
            # print(self.scc_no)

    def find_scc(self):
        self.scc_no = [0]*self.vertex_no
        self.first_visit = [0]*self.vertex_no
        self.scc_adapt = [0]*self.vertex_no
        print(self.scc_no)
        self.scc_cnt = self.dfs_clock = 0
        for i in range(0, self.vertex_no):
            if not self.first_visit[i]:
                self.two_direction_dfs(i)
        # print(self.scc_cnt, self.scc_adapt)


    def build_scc(self):
        self.scc_graph = [[] for _ in range(self.vertex_no+1)]
        for u in range(0, self.vertex_no):
            print("vertex ", u, " is the head of edge # ", self.head[u])
            i = self.head[u]
            print("vertex ", u, "belongs to the scc # ", self.scc_no[u])
            while i != -1:
                v = self.edges[i].to
                if not self.scc_no[u] == self.scc_no[v]:
                    self.scc_graph[self.scc_no[u]].append(self.scc_no[v])
                i = self.edges[i].next
        # print(self.scc_graph)

  
    def bfs_adapt(self):
        bfs_q = []
        visited = [False]*self.vertex_no
        for i in range(1, self.scc_cnt+1):
            self.adapt[i] = self.scc_adapt[i] if self.scc_no[0] == i  else 0
        bfs_q.append(self.scc_no[0])
        while bfs_q != []:
            u = bfs_q.pop(0)
            # print(u)
            visited[u] = False
            for v in self.scc_graph[u]:
                self.adapt[v] = max(self.adapt[v], self.adapt[u] + self.scc_adapt[v])
                if not visited[v]:
                    visited[v] = True 
                    bfs_q.append(v)

    def get_adapt(self):
        return max(self.adapt)
    
    def search_adapt(self):
        self.find_scc()
        self.build_scc()
        self.bfs_adapt()


def main():
    adapt_search = AdaptSearchAlg(4)
    for e in edges:
        adapt_search.create_edge(e)
    adapt_search.search_adapt()
    print(adapt_search.get_adapt())

main()



