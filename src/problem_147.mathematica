(* Project Euler 147: https://projecteuler.net/problem=147

  A cross-hatched m by n grid contains the usual horizontal/vertical lattice lines and both diagonal families in each unit
  cell. For each rectangle size a by b with 1 <= a <= 47 and 1 <= b <= 43, we must count all Euclidean rectangles formed by
  those lines, including axis-aligned and 45-degree tilted ones, and then sum over all sizes.

  Axis-aligned rectangles are classical: choose two vertical and two horizontal grid lines, giving C(m+1,2) C(n+1,2). The
  nontrivial part is tilted rectangles. Introduce transformed coordinates u = x + y and v = x - y. Diagonal lines become
  coordinate lines u = const and v = const, so tilted rectangles become axis-aligned rectangles in the (u,v) system.

  The original box 0 <= x <= m, 0 <= y <= n maps to a convex diamond strip in integer u-levels 0..m+n. For fixed u, valid v
  values form an integer interval [L(u), R(u)] with L(u) = max(-u, u-2n), R(u) = min(2m-u, u). Choosing two u-levels u1 < u2,
  the common admissible v-level count is c = min(R(u1),R(u2)) - max(L(u1),L(u2)) + 1. Each pair of distinct v-levels inside
  that overlap yields one tilted rectangle; contribution is C(c,2) when c >= 2.

  Therefore tilted count for one grid is a double sum over u-pairs, asymptotically O((m+n)^2). The global task over all
  (a,b) up to (47,43) is O(47*43*(47+43)^2), roughly low tens of millions of primitive integer operations, fully practical.
  Memory is O(m+n) per grid from the endpoint arrays.

  Parallelism is naturally coarse-grained by the first dimension a. Each kernel receives a disjoint set of a-values, computes
  full sums over b = 1..43 independently, and returns an exact integer subtotal. Aggregation is associative via Total, so no
  shared mutable state, locks, or race-prone side effects are required.

  The Wolfram Language implementation uses exact integer arithmetic only, compact helper functions for axis and tilted counts,
  and ParallelMap over a-range with deterministic reduction to one final integer. *)

nCores = $ProcessorCount;

ClearAll[axisCount, tiltedCount, totalForWidth, solve];

axisCount[m_Integer, n_Integer] := Binomial[m + 1, 2] Binomial[n + 1, 2];

tiltedCount[m_Integer, n_Integer] := Module[
  {uMax, left, right, total, c},
  uMax = m + n;
  left = Table[Max[-u, u - 2 n], {u, 0, uMax}];
  right = Table[Min[2 m - u, u], {u, 0, uMax}];
  total = 0;
  Do[
    c = Min[right[[u1 + 1]], right[[u2 + 1]]] - Max[left[[u1 + 1]], left[[u2 + 1]]] + 1;
    If[c >= 2, total += Quotient[c (c - 1), 2]],
    {u1, 0, uMax - 1},
    {u2, u1 + 1, uMax}
  ];
  total
];

totalForWidth[m_Integer, nMax_Integer] := Total@Table[
  axisCount[m, n] + tiltedCount[m, n],
  {n, 1, nMax}
];

solve[] := Module[
  {mMax, nMax, widths, partials},
  mMax = 47;
  nMax = 43;
  widths = Range[mMax];
  If[$KernelCount < nCores, LaunchKernels[nCores - $KernelCount]];
  DistributeDefinitions[axisCount, tiltedCount, totalForWidth, nMax];
  partials = ParallelMap[totalForWidth[#, nMax] &, widths, Method -> "FinestGrained"];
  Total[partials]
];

solve[]
