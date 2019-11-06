# Introduction

Main file is goldilocks.R. This calls on design.R, which calls on trial.R.

Amongts other things, you can set enable/disable futility check and parallel processing via the setup function, e.g.

```
setup(run_parallel = 0, enable_fut = 0, ...)

```

The weights to the objective function are also adjusted via `setup`.


# Todo

Convert this to python

This looks interesting:
https://github.com/SheffieldML/GPyOpt/blob/devel/manual/GPyOpt_mixed_domain.ipynb

Do a few basic tutorials on GPy and GPyOpt

# Previous results - ignore

## Using 10000 sims, no futility:

Design maximising objective func

 decision_space index : 63
 objective function   : 5.36996
 nmin                 : 20
 nanalyses            : 5
 zwin                 : 0.95
 wfut                 : 0.15

Operating characteristics
 false +ve            : 0.1227
 expected ss          : 82.637
 expected pwr         : 0.31

Model parameters
 p0                   : 0.4
 p1                   : 0.4, 0.5, 0.55
 

## Revised inputs

Raw Contributions      : 0.95, 0.55, 0.23
Obj func Contributions : 2.86, 0.55, 0.23
Obj func for design 432 : 3.644 false+ 0.053444 ss 45.09 pwr 0.23
Size of decision space : 432
Futility check enabled : 1
Running in parallel    : 1
Objective weights      : 3, 1, 1
Design maximising objective func

 decision_space index : 409
 objective function   : 3.93350013888889
 nmin                 : 20
 nanalyses            : 7
 zwin                 : 0.95
 wfut                 : 0.2

Operating characteristics
 false +ve            : 0.1106
 expected ss          : 30.273
 expected pwr         : 0.268

Model parameters
 p0                   : 0.3, 0.45, 0.6, 0.3, 0.45, 0.6, 0.3, 0.45, 0.6
 p1                   : 0.3, 0.3, 0.3, 0.45, 0.45, 0.45, 0.6, 0.6, 0.6

