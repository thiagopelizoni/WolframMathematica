(* Project Euler Problem 127 - https://projecteuler.net/problem=127

   An abc-hit is a triple of positive integers (a, b, c) with a < b, a + b = c, gcd(a, b) = 1, and rad(abc) < c,
   where rad(n) is the product of distinct prime divisors of n. The problem asks for the sum of all c appearing in
   abc-hits with c < 120000.

   Because c = a + b, coprimality conditions collapse to gcd(a, b) = gcd(a, c) = gcd(b, c). Thus it is enough to test
   gcd(a, c) = 1. Under this coprimality, radicals are multiplicative across the triple:
   rad(abc) = rad(a) rad(b) rad(c), reducing the inequality to rad(a) rad(b) < (c / rad(c)).

   The main preprocessing is a radical sieve up to the limit. For each prime p, multiply p into every multiple of p,
   yielding rad(n) for all n in near-linear time O(N log log N). After that, for each fixed c we enumerate candidate a
   values ordered by increasing rad(a). If lim(c) = floor((c - 1)/rad(c)), only a with rad(a) <= lim(c) can succeed,
   so scanning stops immediately once this bound is exceeded in the radical-ordered list.

   Each surviving candidate checks a < b = c - a, coprimality, and rad(a) rad(b) <= lim(c), which is equivalent to the
   strict target inequality. This pruning is the decisive reduction: most pairs are discarded before gcd evaluation.
   The practical workload is well within Project Euler scale.

   Parallelization is over disjoint c-blocks. Every kernel computes a subtotal of c-contributions independently using
   shared read-only precomputed arrays. Subtotals are combined by Total, an associative reduction, so the computation is
   deterministic and race-free while exploiting all available cores.

   In Wolfram Language, we combine exact integer loops for the sieve and candidate scan, SortBy for radical ordering,
   ParallelMap on c-chunks, and a solve[] wrapper returning one exact integer.
*)

nCores = $ProcessorCount;
LaunchKernels[];

buildRad[limit_Integer?Positive] := Module[{rad, p, m},
  rad = ConstantArray[1, limit];
  For[p = 2, p <= limit, p++,
    If[rad[[p]] == 1,
      For[m = p, m <= limit, m += p,
        rad[[m]] *= p
      ]
    ]
  ];
  rad
]

contributionForC[c_Integer?Positive] := Module[{rc, lim, total, k, a, ra, b},
  rc = radVec[[c]];
  lim = Quotient[c - 1, rc];
  If[lim < 1, Return[0]];
  total = 0;
  k = 1;
  While[k <= lenOrderedA,
    a = orderedA[[k]];
    ra = radVec[[a]];
    If[ra > lim, Break[]];
    b = c - a;
    If[b > a && ra*radVec[[b]] <= lim && CoprimeQ[a, c],
      total += c
    ];
    k++
  ];
  total
]

solve[] := Module[{limit, cValues, chunkSize, chunks, partials},
  limit = 120000;
  radVec = buildRad[limit];
  orderedA = SortBy[Range[1, limit - 1], {radVec[[#]] &, # &}];
  lenOrderedA = Length[orderedA];
  DistributeDefinitions[radVec, orderedA, lenOrderedA, contributionForC];
  cValues = Range[3, limit - 1];
  chunkSize = Max[200, Ceiling[Length[cValues]/(8 nCores)]];
  chunks = Table[cValues[[i ;; Min[i + chunkSize - 1, Length[cValues]]]], {i, 1, Length[cValues], chunkSize}];
  partials = ParallelMap[Total[contributionForC /@ #] &, chunks];
  Total[partials]
]

solve[]
