(* Project Euler Problem 136 - https://projecteuler.net/problem=136

   For n < 50,000,000, count how many integers n admit exactly one triple x > y > z > 0 of consecutive terms in an
   arithmetic progression such that x^2 - y^2 - z^2 = n.

   Write y = a and common difference d > 0. Then x = a + d, z = a - d, and
   n = (a + d)^2 - a^2 - (a - d)^2 = a(4 d - a).
   Put t = 4 d - a. Every solution is equivalent to a factorization n = a t with constraints
   a >= 1, t >= 1, a + t == 0 (mod 4), and t < 3 a (from z = (3 a - t)/4 > 0).
   Hence the representation count r(n) is the number of ordered factor pairs (a, t) of n satisfying these filters.

   This characterization yields a complete classification of r(n) = 1. If n is odd, only odd factors occur, and
   a + t == 0 (mod 4) forces opposite residues 1 and 3 modulo 4. For odd prime p, only (a, t) = (p, 1) can satisfy
   t < 3 a, giving one solution exactly when p == 3 (mod 4). For odd composite n == 3 (mod 4), the pair (n, 1) is valid
   and any odd prime divisor q gives another valid pair (n/q, q), so uniqueness fails.

   If n is even, parity in a + t == 0 (mod 4) implies 4 | n. For n = 4 m with m odd, congruence is automatic after
   writing (a, t) = (2 u, 2 v), uv = m. Uniqueness holds only for m = 1 or odd prime, giving n = 4 and n = 4 p
   with odd prime p. For n divisible by 16, writing (a, t) = (4 u, 4 v), uv = n/16, gives uniqueness only for n = 16
   and n = 16 p with odd prime p. For 8 | n but 16 \[NotVerticalBar] n there are no solutions, and for 32 | n at least
   two solutions exist (for example v = 1 and v = 2 branches), so uniqueness is impossible.

   Therefore n has exactly one solution iff n is in one of these disjoint families:
   odd prime p == 3 (mod 4), or 4 p with odd prime p, or 16 p with odd prime p, plus the base cases 4 and 16.
   With M = 50,000,000 and n < M, this becomes
   count = A + PrimePi[floor((M - 1)/4)] + PrimePi[floor((M - 1)/16)],
   where A is the number of primes p <= M - 1 with p == 3 (mod 4). The two PrimePi terms already incorporate the
   replacements p = 2 -> 4 and p = 2 -> 16, so no extra constant is needed.

   Complexity is dominated by counting A. We compute A by segmented prime ranges, summing local counts of residue class
   3 modulo 4. Prime generation is near-linear in the bound, and counting residues is linear in the number of primes.
   Total work is comfortably feasible for 50 million.

   Parallelization is natural: each segment [lo, hi] is independent. Segments are distributed across all kernels, each
   kernel computes a local integer count, and aggregation is Total, an associative deterministic reduction with no shared
   mutable state.

   The Wolfram Language implementation uses exact integers, PrimeRange for segmented generation, PrimePi for threshold
   prime counts, modular reduction by Mod, and ParallelMap over balanced segments sized from $ProcessorCount.
*)

nCores = $ProcessorCount;
If[Length[Kernels[]] == 0, LaunchKernels[]];
If[Length[Kernels[]] < nCores, LaunchKernels[nCores - Length[Kernels[]]]];
workerCount = Max[1, Length[Kernels[]]];

count3Mod4PrimesInSegment[{lo_Integer, hi_Integer}] := Module[{primes},
  primes = PrimeRange[lo, hi];
  Count[Mod[primes, 4], 3]
]

DistributeDefinitions[count3Mod4PrimesInSegment];

solve[] := Module[{limitExclusive, maxN, segmentCount, segmentSize, segments, aCount, bCount, cCount},
  limitExclusive = 50*10^6;
  maxN = limitExclusive - 1;
  segmentCount = 4 workerCount;
  segmentSize = Ceiling[(maxN - 2)/segmentCount];
  segments = Select[
    Table[
      {3 + (k - 1) segmentSize, Min[maxN, 2 + k segmentSize]},
      {k, 1, segmentCount}
    ],
    #[[1]] <= #[[2]] &
  ];
  aCount = Total[
    ParallelMap[
      count3Mod4PrimesInSegment,
      segments,
      Method -> "CoarsestGrained"
    ]
  ];
  bCount = PrimePi[Quotient[maxN, 4]];
  cCount = PrimePi[Quotient[maxN, 16]];
  aCount + bCount + cCount
]

solve[]
