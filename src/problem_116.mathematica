(* Project Euler Problem 116 — https://projecteuler.net/problem=116
   
   Red, Green or Blue Tiles
   
   A row of n = 50 grey unit squares is to have some of its tiles
   replaced by coloured oblong tiles of a single colour: red
   (length 2), green (length 3), or blue (length 4). Colours may
   not be mixed, and at least one coloured tile must be placed.
   Determine the total number of valid replacements across all
   three colour choices.
   
   Mathematical analysis:
   For a fixed tile length k in {2,3,4}, the number of tilings of
   a row of length n using unit grey squares and coloured tiles of
   length k (including the trivial all-grey tiling) satisfies the
   linear recurrence T(n,k) = T(n-1,k) + T(n-k,k) with initial
   conditions T(i,k) = 1 for 0 <= i < k. This follows from the
   first-cell dichotomy: either cell 1 is grey (leaving n-1 cells)
   or it begins a coloured tile (leaving n-k cells). Unwinding the
   recurrence yields the closed-form binomial sum
   
     T(n, k) = Sum_{j=0}^{floor(n/k)} C(n - (k-1)j, j).
   
   The answer to the problem is
   
     (T(50,2) - 1) + (T(50,3) - 1) + (T(50,4) - 1),
   
   subtracting the all-grey configuration once for each colour.
   
   For k = 2, T(n,2) coincides with the (n+1)-th Fibonacci number,
   so T(50,2) = Fib(51) = 20365011074. For k = 3 and k = 4 the
   sums have at most floor(50/3)+1 = 17 and floor(50/4)+1 = 13
   terms respectively. Each term is a single binomial coefficient
   on modest arguments, so the entire computation is O(n) arithmetic
   on integers of O(n) digits — trivially instantaneous.
   
   Parallelization strategy:
   The three colour computations are completely independent. We
   distribute the definition of the tiling count to all available
   kernels and evaluate the three colour cases in parallel via
   ParallelMap, aggregating the results with Total. Each kernel
   computes one closed-form binomial sum with no shared state.
   
   Implementation plan:
   Detect cores via $ProcessorCount and launch parallel kernels.
   Define tileCount[n,k] as the binomial sum minus 1 (excluding the
   all-grey case). Distribute this definition, then ParallelMap over
   {2,3,4} and return the Total.
*)

nCores = $ProcessorCount;
LaunchKernels[];

tileCount[n_, k_] :=
  Sum[Binomial[n - (k - 1) j, j], {j, 0, Floor[n/k]}] - 1

DistributeDefinitions[tileCount];

solve[] := Module[{n = 50},
  Total[ParallelMap[tileCount[n, #] &, {2, 3, 4}]]
]

solve[]
