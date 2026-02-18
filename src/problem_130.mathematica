(* Project Euler Problem 130 - https://projecteuler.net/problem=130

   Define R(k) = (10^k - 1)/9. For integers n with gcd(n, 10) = 1, let A(n) be the least k such that n divides R(k).
   The problem asks for the sum of the first 25 composite integers n satisfying divisibility of n - 1 by A(n).

   The repunit condition is naturally modular: n | R(k) iff 10^k congruent to 1 modulo 9 n, because
   10^k - 1 = 9 R(k). Hence A(n) is the multiplicative order of 10 modulo 9 n. This removes explicit big-repunit
   construction and replaces it by finite group order computations in (Z/(9 n)Z)^x.

   For primes p > 5, A(p) divides p - 1 by Fermat-type order arguments; the Euler task isolates the rare composite
   analogues. We therefore scan odd n not divisible by 5, discard primes, compute A(n), and keep exactly those with
   (n - 1) mod A(n) = 0.

   The first 25 hits occur at relatively small n, so a direct search with order computation is efficient. Let H be the
   largest tested candidate. Candidate generation is O(H), primality filtering is sublinear per test in practice, and
   multiplicative order evaluation is fast for these magnitudes. Memory stays O(block size).

   Parallelization is performed on disjoint candidate blocks. Within each block, each n is independent, so kernels run
   the composite and order predicate via ParallelMap. The boolean vector preserves candidate ordering, allowing ordered
   extraction with Pick. Block results are appended sequentially, guaranteeing deterministic global first-occurrence
   semantics while exploiting all cores on the expensive predicate evaluations.

   The Wolfram Language implementation uses exact integer arithmetic, MultiplicativeOrder for A(n), PrimeQ filtering,
   chunked ParallelMap evaluation, and a solve[] driver that returns the required sum.
*)

nCores = $ProcessorCount;
LaunchKernels[];

isEuler130Hit[n_Integer?Positive] := Module[{a},
  If[PrimeQ[n], Return[False]];
  a = MultiplicativeOrder[10, 9 n];
  Mod[n - 1, a] == 0
]

candidateBlock[start_Integer?Positive, blockSize_Integer?Positive] := Select[
  Range[start, start + 2 (blockSize - 1), 2],
  Mod[#, 5] != 0 &
]

DistributeDefinitions[isEuler130Hit, candidateBlock];

solve[] := Module[{target, start, blockSize, candidates, flags, hits, newHits},
  target = 25;
  start = 3;
  blockSize = 200 nCores;
  hits = {};
  While[Length[hits] < target,
    candidates = candidateBlock[start, blockSize];
    flags = ParallelMap[isEuler130Hit, candidates];
    newHits = Pick[candidates, flags, True];
    hits = Join[hits, newHits];
    start += 2 blockSize
  ];
  Total[hits[[1 ;; target]]]
]

solve[]
