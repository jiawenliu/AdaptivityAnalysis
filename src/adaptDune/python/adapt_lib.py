class AdaptType:
    value = 0
    def __init__(self, value = 0) -> None:
        self.value = value
    def __add__(self, other):
        if (isinstance(self.value, str)) or isinstance(other.value, str):
            if (isinstance(self.value, int) and int(self.value) == 0):
                return other
            if (isinstance(other.value, int) and int(other.value) == 0):
                return self
            return AdaptType(str(self.value) + " + " + str(other.value))
        else:
            # print(self.value, other.value, " both are int")
            return AdaptType(self.value + other.value)

    def __radd__(self, other):
        if (isinstance(self.value, str)) or isinstance(other.value, str):
            if (isinstance(self.value, int) and int(self.value) == 0):
                return other
            if (isinstance(other.value, int) and int(other.value) == 0):
                return self
            return AdaptType(str(other.value) + " + " + str(self.value))
        else:
            # print(self.value, other.value, " both are int")
            return AdaptType(self.value + other.value)
    
    def __mul__(self, other):
        if (isinstance(self.value, str)) or isinstance(other.value, str):
            if (isinstance(self.value, int) and int(self.value) == 0) or (isinstance(other.value, int) and int(other.value) == 0):
                return AdaptType(0)
            if (isinstance(self.value, int) and int(self.value) == 1):
                return other
            if (isinstance(other.value, int) and int(other.value) == 1):
                return self
            return AdaptType("(" + str(other.value) + ") * (" + str(self.value) + ")")
        else:
            # print(self.value, other.value, " both are int")
            return AdaptType(self.value * (other.value))

    def adapt_max(self, other):
        if (isinstance(self.value, str)) and isinstance(other.value, str):
            if (isinstance(self.value, int) and int(self.value) == 0):
                return other
            if (isinstance(other.value, int) and int(other.value) == 0):
                return self
            return AdaptType("max(" + str(self.value) + ", " + str(other.value) + ")")
        elif (isinstance(self.value, str)) or (isinstance(other.value, str)):
            return self  if other.value == 0 else other if self.value == 0 else AdaptType("max(" + str(self.value) + ", " + str(other.value) + ")")
            # if other.value == 0:
        # elif (isinstance(other.value, str)) and self.value == 0:
        #     return other
        else:
            return AdaptType(max(self.value, other.value))

    def adapt_min(self, other):
        if (isinstance(self.value, str)) and isinstance(other.value, str):
            if (isinstance(self.value, int) and int(self.value) == 0):
                return self
            if (isinstance(other.value, int) and int(other.value) == 0):
                return other
            return AdaptType("min(" + str(self.value) + ", " + str(other.value) + ")")
        elif (isinstance(self.value, str)) or (isinstance(other.value, str)):
            return self  if other.value == 0 else other if self.value == 0 else AdaptType("min(" + str(self.value) + ", " + str(other.value) + ")")
            # if other.value == 0:
        # elif (isinstance(other.value, str)) and self.value == 0:
        #     return other
        else:
            return AdaptType(min(self.value, other.value))

    # def __lt__(self, other):
    #     if (self.value is int) and (other.value is int):
    #         return self.value < other.value

    # def __le__(self, other):
    #     return self.value <= other.value

    # def __eq__(self, other):
    #     return self.value == other.value

    # def __ne__(self, other):
    #     return self.value != other.value

    # def __gt__(self, other):
    #     return self.value > other.value

    # def __ge__(self, other):
    #     return self.value >= other.value

    # def __str__(self):
    #     return str(self.value)

class Graph:
    weights = [AdaptType(1), AdaptType(1)]
    query = [1, 1]
    edges = [(0, 1)]
    def __init__(self, edges = [(0, 1)],  weights = [AdaptType(1), AdaptType(1)],query = [1, 1]):
        self.weights = weights 
        self.query = query
        self.edges = edges
        self.edge_weights = dict({str(u) + "->" + str(v) : AdaptType(0) for (u, v) in edges})
    
    def get_vertice_num(self):
        return len(self.weights)