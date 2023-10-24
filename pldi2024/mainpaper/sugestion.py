import math

q_num = float(input('query number: '))
a_est = float(input('Aest: '))

a =  math.sqrt(q_num)
b = math.sqrt(math.log(q_num)) * a_est

# if a >> b, DS
# if a << b, GS
# a close to b, TS

print(a)
print(b)