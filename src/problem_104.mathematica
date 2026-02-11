(* Project Euler Problem 104 — https://projecteuler.net/problem=104
 
   Find the least index n such that Fibonacci number F_n has its first nine digits and its last nine digits each
   forming a pandigital permutation of 1..9. Leading digits are probed via the Binet approximation F_n ≈ φ^n / √5;
   taking base-10 logs gives log10(F_n) ≈ n log10(φ) − log10(√5). The fractional part of this quantity determines
   the significand; exponentiating 10^(frac+8) yields the top nine digits. Trailing digits are obtained exactly via
   modular arithmetic mod 10^9 using the fast-doubling Fibonacci recurrence (F_{2k}, F_{2k+1}) computed in O(log n).
 
   Both pandigital checks reduce to testing IntegerDigits with fixed width 9 against the sorted set {1,..,9}. The
   search is a linear scan over n, but each check is O(log n); the known solution lies below 5·10^5, so total cost is
   about 5·10^5 log(5·10^5) modular multiplies plus comparable floating operations—comfortably within milliseconds.
   Memory is constant aside from memoized Fibonacci pairs.
 
   Parallelisation is embarrassingly simple: distribute independent indices n across all kernels using ParallelTable;
   each kernel computes the leading and trailing pandigital predicates independently, emitting n or Nothing; the first
   hit across the aggregated list is the answer. No shared mutable state is required, avoiding race conditions.
 
   Implementation outline: precompute log10(φ) and log10(√5); define a pandigital9? predicate; define leading9 via a
   high-precision fractional log; implement fast-doubling Fibonacci modulo 10^9 with memoization; distribute
   definitions to subkernels; scan a safe bound (500000) in parallel and return the minimal qualifying index as solve[].
 *)

nCores = $ProcessorCount;
LaunchKernels[];

solve[] := Module[{mod, logPhi, logSqrt5, pandigital9Q, leading9, fibPair, hits},
  mod = 10^9;
  logPhi = N[Log[(1 + Sqrt[5])/2]/Log[10], 40];
  logSqrt5 = N[Log[Sqrt[5]]/Log[10], 40];
  pandigital9Q[x_] := Sort[IntegerDigits[x, 10, 9]] === Range[9];
  leading9[n_Integer] := Module[{t = FractionalPart[N[n*logPhi - logSqrt5, 40]]}, Floor[10^(t + 8)]];
  Clear[fibPair];
  fibPair[0] = {0, 1};
  fibPair[n_Integer] := fibPair[n] = Module[{a, b, c, d},
    {a, b} = fibPair[Quotient[n, 2]];
    c = Mod[a*(2*b - a), mod];
    d = Mod[a*a + b*b, mod];
    If[EvenQ[n], {c, d}, {d, Mod[c + d, mod]}]
  ];
  DistributeDefinitions[pandigital9Q, leading9, fibPair, mod];
  hits = ParallelTable[
    If[pandigital9Q[leading9[n]] && pandigital9Q[fibPair[n][[1]]], n, Nothing],
    {n, 3, 500000}
  ];
  First[hits]
]

solve[]