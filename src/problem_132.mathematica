(* Project Euler Problem 132 - https://projecteuler.net/problem=132

   Let R(k) = (10^k - 1)/9. The task is to find the sum of the first forty prime divisors of R(10^9), ordered by size.

   For a prime p not dividing 10, p divides R(k) exactly when 10^k is congruent to 1 modulo p, except that p = 3 needs
   special care because of the denominator 9. A uniform criterion that handles all eligible primes is
   10^k congruent to 1 modulo 9 p, since R(k) is integral and p | R(k) iff 9 p | (10^k - 1) when p may share factors
   with 9. This removes large-number arithmetic and reduces the problem to modular exponentiation tests.

   Hence we scan primes p with p not equal to 2, 5, apply PowerMod[10, 10^9, 9 p] == 1, and collect the first forty
   hits. The expensive step is primality-factor testing over many candidate primes; each test is independent, so the
   computation is embarrassingly parallel.

   Let X be a bound containing at least forty hits. Enumerating all primes up to X costs near O(X/log X) candidates, and
   each modular exponentiation runs in O(log 10^9) modular multiplications via repeated squaring. For the Euler instance
   the fortieth hit is small (around 1.6*10^5), so runtime and memory are comfortably modest.

   Parallelization strategy: generate the prime list up to a trial bound, evaluate the divisibility predicate on all
   candidates with ParallelMap, preserve order, and extract hits with Pick. If fewer than forty hits are found, double
   the bound and repeat. Aggregation is deterministic because order is preserved and reductions are pure.

   The Wolfram Language implementation uses exact integer arithmetic only, Prime and PrimePi for candidate generation,
   PowerMod for fast modular checks, and solve[] returning the required sum as a single integer.
*)

nCores = $ProcessorCount;
LaunchKernels[];

isRepunitFactorPrime[p_Integer?PrimeQ, k_Integer?Positive] := Module[{},
  If[p == 2 || p == 5, Return[False]];
  PowerMod[10, k, 9 p] == 1
]

DistributeDefinitions[isRepunitFactorPrime];

solve[] := Module[{k, target, limit, primes, flags, hits},
  k = 10^9;
  target = 40;
  limit = 200000;
  While[True,
    primes = Prime[Range[PrimePi[limit]]];
    flags = ParallelMap[isRepunitFactorPrime[#, k] &, primes];
    hits = Pick[primes, flags, True];
    If[Length[hits] >= target,
      Return[Total[hits[[1 ;; target]]]]
    ];
    limit *= 2
  ]
]

solve[]
