(* Project Euler Problem 118 — https://projecteuler.net/problem=118
   
   Pandigital Prime Sets
   
   Using all digits 1 through 9 exactly once, concatenated into
   groups to form decimal integers, count how many distinct sets
   of primes can be produced. For instance {2, 5, 47, 89, 631} is
   one such set.
   
   Mathematical analysis:
   We seek the number of set-partitions of {1,...,9} into non-empty
   parts, together with a digit permutation within each part, such
   that every part forms a prime. Since different parts use disjoint
   digit sets they produce distinct numbers, so the "set" condition
   is automatically satisfied.
   
   For each non-empty subset S of {1,...,9} let c(S) be the number
   of primes obtainable by permuting the digits of S. The answer
   equals f({1,...,9}) where f is defined by
   
     f(empty) = 1,
     f(S)     = sum over T containing min(S), T subset of S,
                of c(T) * f(S \ T).
   
   The constraint "T contains min(S)" generates each partition
   exactly once. Representing subsets as 9-bit masks, the DP has
   2^9 = 512 states and total work bounded by 3^9 ~ 20000 — this
   is instantaneous.
   
   A critical pruning: the digit sum 1+...+9 = 45 is divisible by
   nine, so any arrangement of all nine digits gives a multiple of
   nine. More generally, for |S| >= 2 with digit sum divisible by
   three, every permutation yields a composite, so c(S) = 0. This
   eliminates roughly a third of subsets and the majority of large
   permutation enumerations.
   
   The precomputation of c(S) requires enumerating all permutations
   of each subset and testing primality. The total number of
   permutations across all 511 non-empty subsets is bounded by
   sum_{k=1}^{9} C(9,k)*k! < 10^6, and PrimeQ is very fast for
   numbers up to ~10^9.
   
   Parallelization strategy:
   The 511 independent subset prime counts c(S) constitute the
   dominant computation. They are distributed across all available
   cores via ParallelMap. The subsequent bitmask DP is negligible
   and runs sequentially on the aggregated results. No shared
   mutable state is needed.
   
   Implementation plan:
   Detect cores, launch kernels. For each bitmask 1..511, extract
   digits, apply digit-sum mod 3 filter, enumerate permutations,
   count primes via PrimeQ. Store results in a flat list pc.
   Iterate masks 1..511 in increasing order (respects the strict
   subset partial order since T proper subset of S implies T < S
   numerically) computing f via the recurrence, storing in an
   array. Return f[511].
*)

nCores = $ProcessorCount;
LaunchKernels[];

bitsToDigits[mask_] := Select[Range[9], BitAnd[mask, 2^(# - 1)] > 0 &]

countPrimes[mask_] := Module[{d = bitsToDigits[mask], s},
  If[Length[d] == 1, Return[Boole[PrimeQ[d[[1]]]]]];
  s = Total[d];
  If[Mod[s, 3] == 0, Return[0]];
  Count[FromDigits /@ Permutations[d], _?PrimeQ]
]

DistributeDefinitions[bitsToDigits, countPrimes];

solve[] := Module[{pc, fArr, minBit, rest, sub, t, total},
  pc = ParallelMap[countPrimes, Range[511]];
  fArr = ConstantArray[0, 512];
  fArr[[1]] = 1;
  Do[
    minBit = BitAnd[mask, -mask];
    rest = BitXor[mask, minBit];
    sub = rest;
    total = 0;
    While[True,
      t = BitOr[sub, minBit];
      total += pc[[t]] * fArr[[BitXor[mask, t] + 1]];
      If[sub == 0, Break[]];
      sub = BitAnd[sub - 1, rest]
    ];
    fArr[[mask + 1]] = total,
    {mask, 1, 511}
  ];
  fArr[[512]]
]

solve[]
