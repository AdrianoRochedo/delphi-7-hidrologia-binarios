Model name:  '' - run #1    
Objective:   Minimize(R0)
 
SUBMITTED
Model size:       29 constraints,      16 variables,          264 non-zeros.
Constraints:       1 equality,          0 GUB,                  0 SOS.
Variables:         0 integer,           0 semi-cont.,           0 SOS.
 
presolve: OF range identified        -1e+030 to       1e+030
presolve:          1 presolve loops were performed
                   2 empty or redundant constraints...  REMOVED.
 
PRESOLVED
Model size:       27 constraints,      16 variables,          258 non-zeros.
Constraints:       1 equality,          0 GUB,                  0 SOS.
Variables:         0 integer,           0 semi-cont.,           0 SOS.
 
Using DUAL simplex for phase 1 and PRIMAL simplex for phase 2.
 
Start at infeasible basis
Entering dual simplex algorithm
Extrad = -1
row_dual: rhs[17] =           0.450421
		upper bound of basic variable:                     0
col_dual: Entering column 41, reduced cost 1, pivot element value 1
Theta =           0.450421
performiteration: Variable 41 entered basis at iteration 1 at           0.450421
performiteration: Feasibility gap at iteration 1 is            4.20202
dualloop: Objective at iteration        1 is          -0.450421 (  17:   17 <-   41)
row_dual: rhs[11] =          -0.398924
col_dual: Entering column 40, reduced cost 0.0888602, pivot element value -0.0570367
Objective value           -1.07192 at iteration        2.
Theta =            6.99417
performiteration: Variable 40 entered basis at iteration 2 at            6.99417
performiteration: Feasibility gap at iteration 2 is            39.3446
dualloop: Objective at iteration        2 is           -1.07192 (  11:   11 <-   40)
row_dual: rhs[10] =           -1.89647
col_dual: Entering column 32, reduced cost 0.191601, pivot element value -1.43023
Theta =            1.32598
performiteration: Variable 32 entered basis at iteration 3 at            1.32598
performiteration: Feasibility gap at iteration 3 is            13.7189
dualloop: Objective at iteration        3 is           -1.32598 (  10:   10 <-   32)
row_dual: rhs[25] =           -1.32598
col_dual: Entering column 28, reduced cost 0.527232, pivot element value -2.23041
Objective value           -1.63942 at iteration        4.
Theta =           0.594501
performiteration: Variable 28 entered basis at iteration 4 at           0.594501
performiteration: Feasibility gap at iteration 4 is            1.65982
dualloop: Objective at iteration        4 is           -1.63942 (  25:   25 <-   28)
row_dual: rhs[19] =          -0.351203
col_dual: Entering column 43, reduced cost 0.82194, pivot element value -1.38947
Theta =           0.252761
performiteration: Variable 43 entered basis at iteration 5 at           0.252761
performiteration: Feasibility gap at iteration 5 is           0.663221
dualloop: Objective at iteration        5 is           -1.84718 (  19:   19 <-   43)
row_dual: rhs[3] =         -0.0694785
col_dual: Entering column 25, reduced cost 0.0148166, pivot element value -0.489601
Objective value           -1.84928 at iteration        6.
Theta =           0.141909
performiteration: Variable 25 entered basis at iteration 6 at           0.141909
performiteration: Feasibility gap at iteration 6 is          0.0483581
dualloop: Objective at iteration        6 is           -1.84928 (   3:    3 <-   25)
row_dual: rhs[27] =         -0.0427388
col_dual: Entering column 10, reduced cost 1.37891, pivot element value -0.912606
Theta =          0.0468316
performiteration: Variable 10 entered basis at iteration 7 at          0.0468316
performiteration: Feasibility gap at iteration 7 is         0.00607326
dualloop: Objective at iteration        7 is           -1.91386 (  27:   27 <-   10)
row_dual: rhs[11] =        -0.00369076
col_dual: Entering column 29, reduced cost 0.083513, pivot element value -4.19444
Objective value           -1.91393 at iteration        8.
Theta =        0.000879919
performiteration: Variable 29 entered basis at iteration 8 at        0.000879919
performiteration: Feasibility gap at iteration 8 is          0.0026052
dualloop: Objective at iteration        8 is           -1.91393 (  11:   40 <-   29)
row_dual: rhs[23] =         -0.0012141
col_dual: Entering column 11, reduced cost 1.57099, pivot element value -0.962965
Theta =          0.0012608
performiteration: Variable 11 entered basis at iteration 9 at          0.0012608
performiteration: Feasibility gap at iteration 9 is       5.07879e-014
dualloop: Objective at iteration        9 is           -1.91591 (  23:   23 <-   11)
Found feasibility by dual simplex at iteration           9
Extrap count = 0
Entered primal simplex algorithm with feasibility TRUE
col_prim: Column 19 reduced cost =           0.415803
row_prim: 20, pivot size =           0.227989
Objective value          -0.666296 at iteration       10.
Theta =                  0
performiteration: Variable 19 entered basis at iteration 10 at                  0
performiteration: Current objective function value at iteration 10 is          -0.666296
primloop: Objective at iteration       10 is          -0.666296 (  20:   20 <-   19)
col_prim: Column 40 reduced cost =           0.283319
row_prim: 11, pivot size =           0.816474
Theta =          0.0015442
performiteration: Variable 40 entered basis at iteration 11 at          0.0015442
performiteration: Current objective function value at iteration 11 is          -0.664982
primloop: Objective at iteration       11 is          -0.664982 (  11:   11 <-   40)
col_prim: Column 20 reduced cost =         0.00574775
row_prim: 24, pivot size =           0.646242
Objective value           -0.66082 at iteration       12.
Theta =          0.0914038
performiteration: Variable 20 entered basis at iteration 12 at          0.0914038
performiteration: Current objective function value at iteration 12 is           -0.66082
primloop: Objective at iteration       12 is           -0.66082 (  24:   24 <-   20)
col_prim: No positive reduced costs found, optimality!

Value of objective function: 0.468936

Actual values of the variables:
X13                       0.41645
X14                     0.0524863
X8                       0.151084
X12                       0.40939
X1                      0.0232005
X9                       0.416325
 
Primal objective:
 
  Column name                      Value   Objective         Min         Max
  --------------------------------------------------------------------------
  X13                                        1           0.41645                 1           1.75834
  X14                                        1         0.0524863                 1           1.05774
  X15                                       -1                 0                -1            1e+030
  X16                                       -1                 0                -1            1e+030
  X8                                         0                 0         -0.149826           0.02006
  X2                                         0                 0        -0.0103092            1e+030
  X3                                         0                 0        -0.0258054            1e+030
  X4                                         0                 0        -0.0466261            1e+030
  X6                                         0                 0         -0.474401            1e+030
  X7                                         0                 0         -0.962315            1e+030
  X10                                        0                 0         -0.210779            1e+030
  X11                                        0                 0         -0.841244            1e+030
  X12                                        0                 0        -0.0929691        0.00727047
  X1                                         0                 0       -0.00439329         0.0355084
  X5                                         0                 0         -0.112486            1e+030
  X9                                         0                 0        -0.0931823         0.0248589
 
Primal variables:
 
  Column name                      Value       Slack         Min         Max
  --------------------------------------------------------------------------
  X13                                  0.41645                 0           -1e+030            1e+030
  X14                                0.0524863                 0           -1e+030            1e+030
  X15                                        0                 0          -0.41645            1e+030
  X16                                        0                 0        -0.0524863            1e+030
  X8                                  0.151084                 0           -1e+030            1e+030
  X2                                         0         0.0103092        -0.0172293          0.286517
  X3                                         0         0.0258054       -0.00476023          0.114463
  X4                                         0         0.0466261       -0.00241344         0.0633498
  X6                                         0          0.474401         -0.149434        0.00392338
  X7                                         0          0.962315        -0.0551594        0.00225142
  X10                                        0          0.210779          -1.10121         0.0385088
  X11                                        0          0.841244        -0.0419213        0.00224711
  X12                                  0.40939                 0           -1e+030            1e+030
  X1                                 0.0232005                 0           -1e+030            1e+030
  X5                                         0          0.112486         -0.405199        0.00980428
  X9                                  0.416325                 0           -1e+030            1e+030
 
Dual variables:
 
  Row name                         Value       Slack         Min         Max
  --------------------------------------------------------------------------
  R1                                         0          -6.25299           -1e+030            1e+030
  R2                                         0          -1.63877           -1e+030            1e+030
  R3                               -0.00225754                 0         -0.383806                 0
  R4                                         0          -2.20039           -1e+030            1e+030
  R6                                         0          -3.36887           -1e+030            1e+030
  R7                                         0                 0           -1e+030            1e+030
  R8                                         0          -1.82717           -1e+030            1e+030
  R10                                        0          -51.7121           -1e+030            1e+030
  R11                                        0          -14.5084           -1e+030            1e+030
  R12                                        0          -2.56224           -1e+030            1e+030
  R13                              -0.00254775                 0           -2.3133           15.2661
  R14                                        0          -13.8317           -1e+030            1e+030
  R15                                        0         -0.316049           -1e+030            1e+030
  R16                                        0          -75.6661           -1e+030            1e+030
  R17                                        0          -17.9544           -1e+030            1e+030
  R18                                        0          -13.8317           -1e+030            1e+030
  R19                               0.00468469               100           2.17918      2.55948e+011
  R20                                        0           9587.04           -1e+030            1e+030
  R21                                        0           157.893           -1e+030            1e+030
  R22                                        0           48.5005           -1e+030            1e+030
  R23                                        0           264.176           -1e+030            1e+030
  R24                                        0           294.141           -1e+030            1e+030
  R25                              0.000239319                 0          -59.1515           3.44233
  R26                             1.40874e-005                 0           -5.7603           209.673
  R27                                        0            129.62           -1e+030            1e+030
  R28                                        0           8.98477           -1e+030            1e+030
  R29                             4.66876e-006               100      3.90579e-008           4588.89
 
 
Final solution              0.468936 at iteration       12.

Excellent numeric accuracy ||*|| = 7.27596e-012

 Memo: Largest [etaPFI v1.0] inv(B) had 88 NZ entries, 0.9x largest basis.
      In the total iteration count 12, 0 (0.0%) were minor/bound swaps.
      There were 2 refactorizations, 0 triggered by time and 0 by density.
             ... on average 6.0 major pivots per refactorization.
      Total solver time was 0.000 seconds.

Value of objective function: 0.468936

Actual values of the variables:
X13                       0.41645
X14                     0.0524863
X8                       0.151084
X12                       0.40939
X1                      0.0232005
X9                       0.416325
