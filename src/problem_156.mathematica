(* Project Euler 156: https://projecteuler.net/problem=156

  For each nonzero digit d, define f_d(n) as the total number of times d appears in the decimal expansions of all integers from 0 through n.
  Let s(d) be the sum of all n satisfying f_d(n)=n. The task is to compute Sum_{d=1}^9 s(d).

  Direct enumeration of n is hopeless because relevant fixed points reach tens of billions. The key is the positional counting identity for d>0:
  at place value p=10^k, with high=Floor[n/(10 p)], cur=Floor[n/p] mod 10, low=n mod p, the contribution is high p, high p+low+1, or
  (high+1)p according as cur<d, cur=d, or cur>d. Summing this over all k gives f_d(n) in O(log_10 n) exact integer operations.

  Write g_d(n)=f_d(n)-n. Although g_d is not monotone, f_d is monotone nondecreasing. Therefore on any interval I=[a,b], if
  [f_d(a),f_d(b)] and [a,b] are disjoint, then no fixed point exists in I. This yields a branch-and-bound bisection search: recursively split
  only those intervals where the two ranges overlap, and verify isolated points exactly at unit-width leaves.

  A finite global bound is obtained without heuristics. Let L=10^11. In any block of L consecutive integers, each of the lowest 11 decimal
  positions contains digit d exactly 10^10 times, so f_d(n+L)-f_d(n) is at least 11*10^10, hence g_d(n+L)-g_d(n)>=10^10.
  Also g_d(n+1)-g_d(n)>=-1 always. Since g_d(10^11)>=10^10 for d=1..9, the minimum possible value inside the j-th subsequent length-L block is
  at least (j+1)10^10-L; this is strictly positive for j>=10. Consequently every solution lies below 11*10^11, and interval search on
  [0,11*10^11] is complete.

  Complexity is O(V log B) integer work per digit, where B=11*10^11 and V is the number of visited overlapping intervals; V is tiny compared with
  B because most branches are pruned by endpoint-range disjointness. The nine digits are independent subproblems, so the dominant workload
  parallelizes perfectly with associative aggregation: each kernel computes the sum of fixed points for one digit, and the master totals digits.

  The Wolfram Language implementation uses only exact integer arithmetic (Quotient, Mod, Which, While), per-digit memoization of f_d boundary
  values, explicit interval stacks for deterministic search, workload partitioning derived from $ProcessorCount, and ParallelMap aggregation. *)

nCores = $ProcessorCount;

ClearAll[
  countDigitUpTo,
  fixedPointsForDigit,
  solve
];

countDigitUpTo[n_Integer?NonNegative, d_Integer?Positive] := Module[
  {p = 1, q, high, cur, low, total = 0},
  While[
    p <= n,
    q = Quotient[n, p];
    high = Quotient[q, 10];
    cur = Mod[q, 10];
    low = n - q*p;
    total += Which[
      cur < d,
      high*p,
      cur == d,
      high*p + low + 1,
      True,
      (high + 1)*p
    ];
    p *= 10;
  ];
  total
];

fixedPointsForDigit[d_Integer, limit_Integer?Positive] := Module[
  {f, stack, a, b, m, fa, fb, fm, solutions = {}},
  Clear[f];
  f[x_Integer?NonNegative] := f[x] = countDigitUpTo[x, d];
  stack = {{0, limit}};
  While[
    Length[stack] > 0,
    {a, b} = Last[stack];
    stack = Most[stack];
    fa = f[a];
    fb = f[b];
    If[
      fa <= b && fb >= a,
      If[
        b - a <= 1,
        If[fa == a, solutions = Append[solutions, a]];
        If[b != a && fb == b, solutions = Append[solutions, b]],
        m = Quotient[a + b, 2];
        fm = f[m];
        If[fa <= m && fm >= a, stack = Append[stack, {a, m}]];
        If[fm <= b && fb >= m + 1, stack = Append[stack, {m + 1, b}]];
      ];
    ];
  ];
  solutions
];

solve[] := Module[
  {limit = 11*10^11, digits = Range[1, 9], workers, chunks, chunkSums},
  workers = Max[1, nCores - 1];
  chunks = Partition[digits, UpTo[Ceiling[Length[digits]/workers]]];
  DistributeDefinitions[countDigitUpTo, fixedPointsForDigit, limit];
  chunkSums = ParallelMap[
    Total[Map[Total[fixedPointsForDigit[#, limit]] &, #]] &,
    chunks,
    Method -> "FinestGrained"
  ];
  Total[chunkSums]
];

solve[]
