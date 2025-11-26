(*
  Project Euler Problem 71: Ordered Fractions
  URL: https://projecteuler.net/problem=071

  Problem Statement:
  Consider the set of reduced proper fractions n/d for d <= 1,000,000. When listed in ascending order, find the
  numerator of the fraction immediately preceding 3/7.

  Mathematical Analysis:
  We seek the fraction n/d < 3/7 such that n/d is maximized, subject to d <= 1,000,000.
  The inequality n/d < 3/7 implies 7n < 3d. Since n and d are integers, this is equivalent to 7n <= 3d - 1.
  To maximize the fraction n/d for a fixed d, we must choose the largest possible numerator n, which is
  n_max(d) = Floor[(3d - 1) / 7].
  Substituting this back, we maximize the function f(d) = Floor[(3d - 1) / 7] / d over the domain 2 <= d <= 1,000,000.
  
  Number Theoretic Insight:
  Let 3d = 7k + r, where r is the remainder modulo 7. Since 7n <= 3d - 1, we are looking for cases where the
  "gap" 3/7 - n/d is minimized.
  Gap = 3/7 - n/d = (3d - 7n) / 7d.
  Since 7n is the largest multiple of 7 less than 3d, the numerator 3d - 7n is exactly the remainder of 3d modulo 7
  (specifically, the smallest positive residue, so 3d = 1 mod 7 yields numerator 1).
  To minimize the gap r / 7d, we need the smallest remainder r=1 and the largest possible denominator d.
  The condition 3d = 1 (mod 7) implies d = 5 (mod 7).
  The largest d <= 1,000,000 satisfying d = 5 (mod 7) is d = 999,997.
  While this analytic method yields the solution in O(1), we implement a parallel search to demonstrate robust
  computational search techniques suitable for less structured variants of this problem (e.g., non-prime moduli).

  Complexity and Feasibility:
  The search space size is N = 10^6. A linear scan is O(N).
  With N=10^6, the computational cost is trivial (~milliseconds on modern hardware).
  This allows for a straightforward parallel decomposition without complex optimization (like sieves).

  Parallelization Strategy:
  We partition the range [2, 1,000,000] into disjoint chunks based on the number of available processor cores.
  Each core independently scans its assigned range of denominators d, computing the optimal n for each d and tracking
  the local maximum fraction.
  The main process collects the local maxima from all workers and determines the global maximum.
  We use exact rational arithmetic to avoid floating-point precision issues.

  Wolfram Language Implementation:
  - Detect core count.
  - Divide the range 2..1,000,000 into chunks.
  - Define a worker function that iterates through a sub-range and finds the max Rational[(3d-1)/7 // Floor, d].
  - Use ParallelMap to execute workers.
  - Compute the Max of the returned rational numbers.
  - Extract the numerator of the resulting reduced fraction.
*)

solve[] := Module[{nCores, limit, chunkSize, ranges, findMaxInInterval, partialResults, bestFraction},
  nCores = $ProcessorCount;
  limit = 1000000;

  (* Helper function to find the best fraction in a given range of denominators *)
  findMaxInInterval = Function[{range},
    Module[{d, n, currentFrac, bestFrac},
      bestFrac = 0; (* Initialize with a value smaller than any target *)
      Do[
        (* Constraint: n/d < 3/7 => 7n < 3d => n <= floor((3d-1)/7) *)
        n = Quotient[3 * d - 1, 7];
        
        (* Form the rational number. Mathematica automatically reduces fractions, but comparison is exact. *)
        currentFrac = n / d;
        
        (* Update local maximum *)
        If[currentFrac > bestFrac, bestFrac = currentFrac],
        
        {d, range[[1]], range[[2]]}
      ];
      bestFrac
    ]
  ];

  (* Partition the work. Divide the range [2, limit] into nCores chunks *)
  (* Using Ceiling to ensure coverage, and limiting the last chunk to 'limit' *)
  chunkSize = Ceiling[(limit - 1) / nCores];
  ranges = Table[
    {2 + (i - 1) * chunkSize, Min[2 + i * chunkSize - 1, limit]},
    {i, 1, nCores}
  ];

  (* Execute the search in parallel *)
  partialResults = ParallelMap[findMaxInInterval, ranges];

  (* Aggregate results: find the maximum among the candidates returned by each core *)
  bestFraction = Max[partialResults];

  (* Return the numerator of the optimal reduced fraction *)
  Numerator[bestFraction]
]

solve[]