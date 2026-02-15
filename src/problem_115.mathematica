(* Project Euler Problem 115 — https://projecteuler.net/problem=115
   
   Counting Block Combinations II
   
   A row of n unit cells is tiled with "red" blocks of length >= m,
   each pair separated by at least one "black" (empty) cell; any
   remaining cells are black. Let F(m,n) be the number of such
   tilings. Given that F(3,30) is the first n exceeding one million
   for m=3 and F(10,57) is the first for m=10, find the least n
   with F(50,n) > 1 000 000.
   
   Mathematical analysis:
   Fix m and place exactly r red blocks of lengths l_1,...,l_r >= m
   on a row of length n, with at least one black cell between
   consecutive blocks. Substituting l_i = m + e_i (e_i >= 0) and
   inserting mandatory unit gaps between blocks transforms the
   problem into distributing excess lengths and surplus black cells
   among r+1+r bins. By a double application of the Vandermonde-Chu
   identity the resulting convolution collapses to a single binomial
   coefficient. Summing over the number of blocks r gives the
   closed form
   
     F(m, n) = Sum_{r=0}^{floor((n+1)/(m+1))} C(n - r(m-1) + 1, 2r).
   
   This is easily verified against the stated test values
   F(3,29) = 673135 and F(10,56) = 880711. For m = 50 the upper
   summation limit is at most floor((n+1)/51), so each evaluation
   of F costs O(n/m) arithmetic on integers of moderate size —
   effectively O(1) for the range of interest. F is monotonically
   increasing in n, so we seek the first crossing of the
   threshold 10^6.
   
   Parallelization strategy:
   Because evaluations of F(50,n) for distinct n share no state, we
   compute F(50,n) for a generous range of candidate n values in one
   ParallelTable call, distributing the range evenly across all
   available cores. The results are aggregated into a flat list and
   a single LengthWhile scan locates the threshold crossing. The
   parallel step dominates wall-clock time and scales linearly with
   the number of cores.
   
   Implementation plan:
   Define fillCount[m,n] using the closed-form sum of Binomial
   coefficients, distribute this definition to parallel kernels,
   compute a table of counts, then extract the first index exceeding
   one million. All arithmetic is exact integer arithmetic. No
   printing or intermediate output is produced; the final expression
   evaluates to the answer.
*)

nCores = $ProcessorCount;
LaunchKernels[nCores];

fillCount[m_, n_] :=
  Sum[Binomial[n - r (m - 1) + 1, 2 r], {r, 0, Floor[(n + 1)/(m + 1)]}]

DistributeDefinitions[fillCount];

solve[] := Module[{m = 50, target = 1000000, nMin, nMax, results},
  nMin = m;
  nMax = 300;
  results = ParallelTable[fillCount[m, n], {n, nMin, nMax}];
  nMin + LengthWhile[results, # <= target &]
]

solve[]
