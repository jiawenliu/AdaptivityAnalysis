# empirical_adaptive_data_analysis

Library containing an implementation of the Guess and Check framework.

## execution:
`$cd src`

`$python adapt-test.py`

## Check files: 
1. results/test.txt
2. plots/test.png

if generated correctly, then execute:

`$python adapt.py`

## To switch mechanisms:

open file adapt.py

at Line:252, choose the Mechanism, add the mechanism name into the mechs list

run in terminal

`$python adapt.py`

# Evaluation of The Generalization Error on Benchmarks.

`$cd cd adapt-GnC/src/examples`

## choose Adaptive Data Analysis Algorithm and Execute.

Each python file contains the implementation of one mechanism, for example, if you want to play with two_rounds mechanism, run the following commands in the terminal:

`$python two_rounds.py`

**Tune Parameter**
In each file, the parameters are set at lines around #100 ~ #120 in form of:
```
'''
Parameters of Strategies
'''
n = 1000
dimension = 1
q_max = 1000


'''
Parameters of Mechanisms
'''
beta, tau = 0.05, 1.0
sigma = 1.0
hold_frac, threshold, check_data_frac = 0.7, 0.05, 0.05
```
Modifying them and then play with the mechanisms.