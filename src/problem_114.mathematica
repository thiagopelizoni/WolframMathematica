(* Project Euler Problem 114 — https://projecteuler.net/problem=114
   
   Counting Block Combinations I
   
   A row of length n is to be filled with black unit squares and red
   blocks of length at least 3, subject to the constraint that any two
   red blocks must be separated by at least one black unit square.
   Determine the total number of valid tilings for n = 50.
   
   Mathematical analysis:
   A tiling is uniquely specified by the number k of red blocks, their
   lengths m_1, ..., m_k (each >= 3), and the gaps between them. Let
   g_0 >= 0 be the leading gap, g_1, ..., g_{k-1} >= 1 be the internal
   gaps, and g_k >= 0 be the trailing gap, so that
   
     g_0 + m_1 + g_1 + m_2 + ... + g_{k-1} + m_k + g_k = n.
   
   Substitute m_i' = m_i - 3 >= 0 and g_j' = g_j - 1 >= 0 for the
   internal gaps (j = 1, ..., k-1). The constraint becomes
   
     m_1' + ... + m_k' + g_1' + ... + g_{k-1}' + g_0 + g_k
       = n - 3k - (k - 1) = n + 1 - 4k.
   
   This is a stars-and-bars problem with 2k + 1 non-negative integer
   unknowns and right-hand side n + 1 - 4k, yielding
   
     C(n + 1 - 4k + 2k, 2k) = C(n + 1 - 2k, 2k)
   
   solutions, provided n + 1 - 4k >= 0, i.e. k <= floor((n+1)/4).
   Summing over all valid k gives the closed-form expression
   
     f(n) = Sum_{k=0}^{floor((n+1)/4)} C(n + 1 - 2k, 2k).
   
   For n = 50, the upper limit is k = 12, yielding 13 independent
   binomial coefficient evaluations. Each evaluation is O(k) arithmetic
   operations on integers of O(n log n) bits, so the total work is
   O(n^2 log n) bit-operations — trivially feasible for n = 50.
   
   An equivalent linear recurrence f(n) = 2f(n-1) - f(n-2) + f(n-4)
   can be derived by differencing the summation formula; both approaches
   yield O(n) term evaluations.
   
   Parallelization strategy:
   The closed-form sum decomposes into 13 completely independent
   binomial coefficient evaluations, one per value of k in {0,...,12}.
   These are distributed across all available CPU cores via
   ParallelTable and aggregated with Total. No synchronisation or
   shared mutable state is needed, as each term depends only on k
   and the fixed parameter n.
   
   Implementation plan:
   Detect available cores via $ProcessorCount and launch parallel
   kernels. Compute each Binomial[n+1-2k, 2k] independently using
   ParallelTable over k = 0 to Floor[(n+1)/4]. Return Total of the
   resulting list as the final answer.
*)

nCores = $ProcessorCount;
LaunchKernels[];

solve[] := Module[{n = 50, maxK},
  maxK = Floor[(n + 1)/4];
  Total[ParallelTable[Binomial[n + 1 - 2 k, 2 k], {k, 0, maxK}]]
]

solve[]