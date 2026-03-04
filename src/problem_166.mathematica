(* Project Euler 166: https://projecteuler.net/problem=166

We count 4x4 grids of decimal digits where every row sum, every column sum, and both main diagonals share a common value S.
Digits are in {0,...,9}, so S lies in 0..36. The naive space has 10^16 assignments, but all constraints are linear and highly coupled.

Write the grid rows as r1,r2,r3,r4. For fixed S, each row must be a length-4 digit vector summing to S. Let N_S be the number of such rows.
Instead of assembling all quadruples of rows, we split into top pair (r1,r2) and bottom pair (r3,r4). For an ordered pair (x,y), define
column pair sums u_c=x_c+y_c for c=1..4. Also define top diagonal partials t1=x_1+y_2 and t2=x_4+y_3, and bottom partials
b1=x_3+y_4 and b2=x_2+y_1. A full grid is valid iff bottom data equals the complement of top data:
u'_c=S-u_c, b1=S-t1, b2=S-t2. Therefore, for each S, the count is a convolution over signatures in Z^6:
sum_{k} TopCount_S(k) * BottomCount_S(Comp_S(k)).

This replaces an intractable O(N_S^4) search by O(N_S^2) signature generation plus hash aggregation. Summed over S, complexity is
O(sum_S N_S^2), and here sum_S N_S^2 is only a few million pair evaluations, fully feasible with exact integer arithmetic.
No floating-point operations are used, and deduplication is purely structural via integer-keyed associations.

Parallelization is exact and embarrassingly parallel across S in 0..36: each sum value defines an independent counting problem with no shared
mutable state. Workers compute per-S totals separately, then Total combines partial results through associative addition.
This partition is balanced enough in practice because large N_S values are concentrated near central sums and are naturally distributed.

The Wolfram Language implementation builds all rows grouped by S, computes two signature multisets with Counts for each S, performs the
complement lookup convolution, and aggregates across sums through ParallelMap with a deterministic serial fallback when subkernels are absent. *)

nCores = $ProcessorCount;

ClearAll[buildRowsBySum, countForSum, solve];

buildRowsBySum[] := Module[
  {rowGroups},
  rowGroups = GroupBy[
    Tuples[Range[0, 9], 4],
    Total
  ];
  Table[
    Lookup[rowGroups, s, {}],
    {s, 0, 36}
  ]
];

countForSum[s_Integer, rowsBySum_List] := Module[
  {rows, topKeys, bottomKeys, topCounts, bottomCounts},
  rows = rowsBySum[[s + 1]];
  If[
    rows === {},
    Return[0]
  ];
  topKeys = Flatten[
    Table[
      {
        r1[[1]] + r2[[1]],
        r1[[2]] + r2[[2]],
        r1[[3]] + r2[[3]],
        r1[[4]] + r2[[4]],
        r1[[1]] + r2[[2]],
        r1[[4]] + r2[[3]]
      },
      {r1, rows},
      {r2, rows}
    ],
    1
  ];
  bottomKeys = Flatten[
    Table[
      {
        r1[[1]] + r2[[1]],
        r1[[2]] + r2[[2]],
        r1[[3]] + r2[[3]],
        r1[[4]] + r2[[4]],
        r1[[3]] + r2[[4]],
        r1[[2]] + r2[[1]]
      },
      {r1, rows},
      {r2, rows}
    ],
    1
  ];
  topCounts = Counts[topKeys];
  bottomCounts = Counts[bottomKeys];
  Total[
    Map[
      With[
        {v = bottomCounts[s - #[[1]]]},
        #[[2]]*If[MissingQ[v], 0, v]
      ] &,
      Normal[topCounts]
    ]
  ]
];

solve[] := Module[
  {rowsBySum, sums, parallelResult, partialCounts},
  rowsBySum = buildRowsBySum[];
  sums = Range[0, 36];
  partialCounts = If[
    nCores > 1,
    Module[{},
      parallelResult = $Failed;
      Quiet[
        Check[
          If[
            Length[LaunchKernels[]] > 0,
            DistributeDefinitions[countForSum, rowsBySum];
            parallelResult = ParallelMap[
              countForSum[#, rowsBySum] &,
              sums,
              Method -> "CoarsestGrained"
            ]
          ],
          parallelResult = $Failed
        ]
      ];
      If[
        parallelResult === $Failed,
        Map[
          countForSum[#, rowsBySum] &,
          sums
        ],
        parallelResult
      ]
    ],
    Map[
      countForSum[#, rowsBySum] &,
      sums
    ]
  ];
  Total[partialCounts]
];

solve[]
