(*
  Project Euler Problem 72: Counting Fractions
  URL: https://projecteuler.net/problem=072

  Problem Statement:
  Consider the set of reduced proper fractions n/d for d <= 1,000,000. A reduced proper fraction is a fraction n/d
  where HCF(n, d) = 1 and n < d. Determine the total number of elements in this set.

  Mathematical Analysis:
  The number of reduced proper fractions for a fixed denominator d is exactly the count of numerators n such that
  1 <= n < d and gcd(n, d) = 1. This is the definition of Euler's totient function, phi(d).
  Therefore, the total number of reduced proper fractions for d <= L is the sum of phi(d) for d from 2 to L.
  (d=1 contributes 0 since n < d is impossible for positive integers n).
  We must compute S = Sum[phi(d), {d, 2, 1000000}].

  Complexity and Feasibility:
  The input limit is L = 10^6. While sublinear algorithms O(L^(2/3)) exist (using Dirichlet hyperbola method/Mobius
  inversion), the linear O(L) approach—or slightly higher depending on phi implementation—is extremely efficient for
  L = 10^6. Specifically, computing phi(d) for all d up to L can be done efficiently using a sieve or, in Mathematica,
  using the built-in vectorized EulerPhi function. The overhead of the sublinear recursive method would likely exceed
  the direct summation cost for this specific L.
  The time complexity will be effectively O(L) with a small constant factor due to optimized built-ins. Memory is O(L)
  to hold the range or O(L/P) per core.

  Parallelization Strategy:
  The summation is associative and commutative. We can decompose the range [2, L] into disjoint sub-intervals.
  Each available CPU core will process one or more intervals. Inside each interval, we utilize Mathematica's vectorized
  arithmetic (applying EulerPhi to a Range list), which minimizes interpreter overhead and leverages low-level optimization.
  The partial sums from each core are then aggregated to form the final result.

  Wolfram Language Implementation:
  - Detect the number of processor cores.
  - Partition the domain [2, 1,000,000] into chunks relative to the number of cores.
  - Use ParallelMap to apply a worker function to each chunk.
  - The worker function generates the integer range, applies EulerPhi vector-wise, and computes the Total.
  - Sum the results from all workers.
*)

solve[] := Module[{limit, nCores, chunkSize, ranges, worker, partialSums},
  limit = 1000000;
  nCores = $ProcessorCount;

  worker = Function[{bounds},
    Total[EulerPhi[Range[bounds[[1]], bounds[[2]]]]]
  ];

  chunkSize = Ceiling[(limit - 1) / nCores];

  ranges = Table[
    {2 + (i - 1) * chunkSize, Min[2 + i * chunkSize - 1, limit]},
    {i, 1, nCores}
  ];

  ranges = Select[ranges, #[[1]] <= #[[2]] &];

  partialSums = ParallelMap[worker, ranges];

  Total[partialSums]
]

solve[]