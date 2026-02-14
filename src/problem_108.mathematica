(*
Project Euler 108 — https://projecteuler.net/problem=108

We seek the least positive integer n for which the Diophantine equation 1/x + 1/y = 1/n has more than 1000
distinct solutions in positive integers, counting each unordered pair once (equivalently x >= y).

Rewrite as (x - n) (y - n) = n^2. Thus each factorization n^2 = d1 d2 with d1 <= d2 gives one solution, so the
number of distinct solutions is (tau(n^2) + 1)/2, where tau is the divisor-counting function. Therefore the target is
tau(n^2) > 1999.

If n = Product[p_i^a_i], then tau(n^2) = Product[(2 a_i + 1)]. The optimization is now: minimize n subject to
Product[(2 a_i + 1)] > 1999. Standard majorization and rearrangement arguments imply that for a minimal n the
exponents are nonincreasing on increasing primes: a1 >= a2 >= ... >= ak >= 1. Hence the search space is finite and can
be traversed by branch-and-bound over exponent vectors.

A practical upper bound is obtained from seven distinct prime factors with exponent 1:
tau(n^2) = 3^7 = 2187 > 1999, so n <= 2*3*5*7*11*13*17 = 510510. This bound makes pruning effective: any partial
assignment whose current n already exceeds the best known value is discarded, and exponent growth is capped by this
bound. The explored tree is tiny versus naive integer scanning; complexity is governed by the count of admissible
nonincreasing exponent tuples below the incumbent bound, which is very small for this threshold.

Parallelization is embarrassingly parallel by fixing the first exponent a1 and exploring each subtree independently.
Each kernel performs a complete depth-first search on its disjoint branch and returns a local minimum. Aggregation is a
single associative reduction via Min over branch results, so no race-prone shared mutable state is required.

The Wolfram Language implementation uses exact integer arithmetic only, local recursive search inside Module, explicit
prime lists, and ParallelMap for the dominant workload. Determinism follows from fixed traversal rules and pure
integer operations.
*)

ClearAll[solve];

solve[] := Module[
  {
    target = 1000,
    threshold = 2*1000 - 1,
    nCores,
    primes,
    upperBound,
    firstExpMax,
    firstExponents,
    branchResults
  },
  nCores = $ProcessorCount;
  LaunchKernels[nCores];
  primes = Prime[Range[16]];
  upperBound = Times @@ Prime[Range[7]];
  firstExpMax = Floor[Log[2, upperBound]];
  firstExponents = Select[Range[firstExpMax], 2^# < upperBound &];
  DistributeDefinitions[threshold, primes, upperBound];
  branchResults = ParallelMap[
    Function[firstExp,
      Module[{best = upperBound, limit = Length[primes], recur},
        recur[idx_, maxExp_, currentN_, currentTau_] := Module[{p, eMax, e, n2, tau2},
          If[currentTau > threshold,
            If[currentN < best, best = currentN];
            Return[];
          ];
          If[idx > limit, Return[]];
          p = primes[[idx]];
          eMax = Min[maxExp, Floor[Log[p, best/currentN]]];
          For[e = eMax, e >= 1, e--,
            n2 = currentN*p^e;
            tau2 = currentTau*(2*e + 1);
            If[tau2 > threshold,
              If[n2 < best, best = n2],
              If[idx < limit && n2*primes[[idx + 1]] < best, recur[idx + 1, e, n2, tau2]]
            ];
          ];
        ];
        recur[2, firstExp, 2^firstExp, 2*firstExp + 1];
        best
      ]
    ],
    firstExponents,
    Method -> "CoarsestGrained"
  ];
  Min[branchResults]
]

solve[]