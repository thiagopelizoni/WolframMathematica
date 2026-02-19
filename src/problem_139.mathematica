(* Project Euler Problem 139 - https://projecteuler.net/problem=139

   For integer right triangles (a, b, c) with perimeter below 10^8, count those for which the classic four-triangle
   arrangement inside a c-by-c square leaves a central square of side |a - b| that tiles the outer square exactly.
   This tiling criterion is equivalent to divisibility c == 0 (mod |a - b|).

   Reduce first to primitive triples. If gcd(a, b, c) = 1 and |a - b| divides c, then any common divisor of c and |a - b|
   also divides b and a, hence must be 1; therefore |a - b| = 1 in the primitive case. So primitive admissible triples are
   precisely those with consecutive legs.

   Write legs as x and x + 1. Then c^2 = x^2 + (x + 1)^2 = 2 x^2 + 2 x + 1. With y = 2 x + 1 this becomes
   y^2 - 2 c^2 = -1, a negative Pell equation. Nondegenerate primitive solutions correspond to positive Pell solutions
   beyond (y, c) = (1, 1), namely (7, 5), (41, 29), ... . Their perimeters p = x + (x + 1) + c = y + c satisfy the
   linear recurrence p_{n+1} = 6 p_n - p_{n-1} with seeds p_1 = 12, p_2 = 70.

   Any non-primitive multiple k(a, b, c) preserves c/|a - b|, so every multiple remains valid. Thus each primitive
   perimeter p contributes exactly Floor[(10^8 - 1)/p] scaled triangles, and the total answer is the sum of these terms
   over all primitive perimeters below 10^8.

   The recurrence grows like (3 + 2 Sqrt[2])^n, so only O(log limit) primitive perimeters exist; runtime and memory are
   negligible compared with brute-force enumeration over Euclid parameters.

   Parallelization is embarrassingly parallel over primitive perimeters: each perimeter contributes an independent integer
   quotient. We map these quotient computations across all kernels and combine with Total, an associative deterministic
   reduction without shared mutable state.

   The Wolfram Language implementation uses exact integer recurrence generation, Quotient for floor division, ParallelMap
   for multicore decomposition, and a single solve[] function returning the exact Project Euler value.
*)

nCores = $ProcessorCount;
If[Length[Kernels[]] == 0, LaunchKernels[]];
If[Length[Kernels[]] < nCores, LaunchKernels[nCores - Length[Kernels[]]]];

primitivePerimeters[limit_Integer?Positive] := Module[{p1, p2},
  p1 = 12;
  p2 = 70;
  Reap[
    While[p1 < limit,
      Sow[p1];
      {p1, p2} = {p2, 6 p2 - p1};
    ]
  ][[2, 1]]
]

DistributeDefinitions[primitivePerimeters];

solve[] := Module[{limit, perimeters, contributions},
  limit = 10^8;
  perimeters = primitivePerimeters[limit];
  contributions = ParallelMap[
    Quotient[limit - 1, #] &,
    perimeters,
    Method -> "CoarsestGrained"
  ];
  Total[contributions]
]

solve[]
