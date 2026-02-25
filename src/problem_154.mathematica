(* Project Euler 154: https://projecteuler.net/problem=154

  In the trinomial expansion (x + y + z)^200000, coefficients are multinomial values N!/(a! b! c!) with a+b+c=N. The task is to count
  how many are divisible by 10^12, equivalently by both 2^12 and 5^12.

  For each prime p, Legendre yields v_p(n!) = Sum[floor(n/p^k), k>=1]. Therefore
  v_p(N!/(a! b! c!)) = v_p(N!) - v_p(a!) - v_p(b!) - v_p(c!).
  With N fixed, precomputing factorial-prefix valuations F_p(k)=v_p(k!) reduces each test to constant-time integer arithmetic.

  A direct scan over all (a,b,c) with a+b+c=N is quadratic in N and too expensive unless symmetry and arithmetic structure are used.
  Write coefficients as C(N,i) C(i,j), where i=a+b and j=a. Then i ranges 0..N and j ranges 0..i. For a fixed i, the first factor has
  valuations v_p(C(N,i)); the second contributes v_p(C(i,j)). Thus each coefficient test becomes two prefix-difference evaluations.

  The row symmetry C(i,j)=C(i,i-j) allows scanning only j<=i/2 and doubling non-central hits. This halves the dominant work exactly while
  preserving determinism. The asymptotic cost remains Theta(N^2), but with tight constant factors, branch pruning when the first factor
  already satisfies both thresholds, and compiled machine-integer loops the instance N=200000 is computationally feasible.

  Parallelization is embarrassingly clean in the outer index i: disjoint i-intervals are independent subproblems. We partition 0..N into
  contiguous chunks, map chunk counters to kernels, and aggregate with Total, an associative reduction free of race conditions.
  Kernel launch is attempted dynamically up to $ProcessorCount - 1 with a bounded timeout; if unavailable, execution falls back to serial
  evaluation without changing results.

  The Wolfram Language implementation uses exact integer arithmetic, packed arrays for F_2 and F_5, and a compiled chunk counter
  (CompilationTarget -> "C") for the tight nested loops. No external files, randomness, or mutable shared parallel state are used,
  so the computation is reproducible and deterministic. *)

nCores = $ProcessorCount;

ClearAll[
  countPrimeExponent,
  buildPrimeFactorialPrefix,
  chunkCounter,
  solve
];

countPrimeExponent[p_Integer, n_Integer] := Module[
  {x = n, e = 0},
  While[
    x > 0 && Mod[x, p] == 0,
    e++;
    x = Quotient[x, p];
  ];
  e
];

buildPrimeFactorialPrefix[p_Integer, n_Integer] := Module[
  {prefix, k},
  prefix = ConstantArray[0, n + 1];
  For[k = 1, k <= n, k++,
    prefix[[k + 1]] = prefix[[k]] + countPrimeExponent[p, k];
  ];
  Developer`ToPackedArray[prefix]
];

chunkCounter = Compile[
  {{sum2, _Integer, 1}, {sum5, _Integer, 1}, {n, _Integer}, {threshold, _Integer}, {from, _Integer}, {to, _Integer}},
  Module[
    {result = 0, i, j, first2, first5, second2, second5, lim, s2i, s5i},
    For[i = from, i <= to, i++,
      first2 = sum2[[n + 1]] - (sum2[[n - i + 1]] + sum2[[i + 1]]);
      first5 = sum5[[n + 1]] - (sum5[[n - i + 1]] + sum5[[i + 1]]);
      If[
        first2 >= threshold && first5 >= threshold,
        result += i + 1,
        s2i = sum2[[i + 1]];
        s5i = sum5[[i + 1]];
        lim = Quotient[i, 2];
        For[j = 0, j <= lim, j++,
          second2 = first2 + s2i - (sum2[[i - j + 1]] + sum2[[j + 1]]);
          second5 = first5 + s5i - (sum5[[i - j + 1]] + sum5[[j + 1]]);
          If[
            second2 >= threshold && second5 >= threshold,
            result++;
            If[j < i - j, result++];
          ];
        ];
      ];
    ];
    result
  ],
  CompilationTarget -> "C",
  RuntimeOptions -> "Speed"
];

solve[] := Module[
  {n = 200000, threshold = 12, sum2, sum5, kernelsTarget, workers, chunkSize, ranges, partials},
  sum2 = buildPrimeFactorialPrefix[2, n];
  sum5 = buildPrimeFactorialPrefix[5, n];
  kernelsTarget = Max[0, nCores - 1];
  If[
    kernelsTarget > $KernelCount,
    TimeConstrained[
      Block[
        {$Messages = {}},
        Check[
          LaunchKernels[kernelsTarget - $KernelCount],
          Null
        ]
      ],
      10,
      Null
    ]
  ];
  workers = Max[1, Min[kernelsTarget, Length[Kernels[]]]];
  chunkSize = Max[1, Ceiling[(n + 1)/workers]];
  ranges = Table[
    {start, Min[n, start + chunkSize - 1]},
    {start, 0, n, chunkSize}
  ];
  partials = If[
    workers > 1,
    DistributeDefinitions[chunkCounter, sum2, sum5, n, threshold];
    ParallelMap[
      chunkCounter[sum2, sum5, n, threshold, #[[1]], #[[2]]] &,
      ranges,
      Method -> "CoarsestGrained"
    ],
    Map[
      chunkCounter[sum2, sum5, n, threshold, #[[1]], #[[2]]] &,
      ranges
    ]
  ];
  Total[partials]
];

solve[]
