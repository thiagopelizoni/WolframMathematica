(* Project Euler 141: https://projecteuler.net/problem=141
   We must sum all integers n < 10^12 that are both perfect squares and progressive.
   Progressive means that n = d q + r with 0 < r < d, and d, q, r are three consecutive terms
   of a geometric progression, without fixing which symbol is first, second, or third.
   Any integer geometric triple can be written as a b^2, a b c, a c^2 with c > b > 0 and
   gcd(b, c) = 1 after reducing the ratio. Since r < d, the remainder is not the largest term.
   If r were the middle term, then n would be t (t + 1) with t = a b c, impossible for a
   positive square because consecutive integers are coprime and cannot both be nontrivial squares.
   Hence every progressive square comes from r = a b^2 and {d, q} = {a b c, a c^2}, giving
   n = a b (a c^3 + b). This is complete for square targets under the progressive constraint.
   For fixed c and b, n grows strictly with a. From n >= a^2 b c^3 we get
   a <= floor(sqrt((N - 1)/(b c^3))) for N = 10^12. Also at a = 1 we need b (c^3 + b) < N,
   so b <= min(c - 1, floor((N - 1)/c^3)), which sharply truncates b when c is large.
   Therefore c only runs to N^(1/3), and the candidate count is
   sum_{c,b} O(sqrt(N/(b c^3))) = O(N^(1/2) log N), comfortably within Project Euler scale.
   Parallelization splits independent c-subproblems across all kernels. Small c values are heavier,
   so fine-grained dynamic scheduling is used to keep cores balanced. Each kernel emits local hits,
   and final aggregation is deterministic with Flatten, Union, and Total, avoiding shared state.
   The Wolfram Language implementation uses exact integer arithmetic throughout: CoprimeQ for reduced
   ratios, exact integer roots via Floor[Sqrt[...]] for bounds and square testing, Reap/Sow for local collection,
   and ParallelMap over c. The script is deterministic and self-contained. *)

nCores = $ProcessorCount;
If[Length[Kernels[]] < nCores,
  LaunchKernels[nCores - Length[Kernels[]]]
];

iSqrt[n_Integer] := Floor[Sqrt[n]];

squareQ[n_Integer] := Module[{s = iSqrt[n]},
  s s == n
];

solve[] := Module[
  {limit = 10^12, cMax = 1, perC, squares},
  While[(cMax + 1)^3 < limit,
    cMax++
  ];
  DistributeDefinitions[iSqrt, squareQ];
  perC = With[{limitLocal = limit},
    ParallelMap[
      Function[c,
        Module[{c3 = c^3, bMax, bucket},
          bMax = Min[c - 1, Quotient[limitLocal - 1, c3]];
          bucket = Reap[
            Do[
              If[CoprimeQ[b, c],
                Module[{aMax, n},
                  aMax = iSqrt[Quotient[limitLocal - 1, b c3]];
                  Do[
                    n = a b (a c3 + b);
                    If[n < limitLocal && squareQ[n],
                      Sow[n]
                    ],
                    {a, 1, aMax}
                  ]
                ]
              ],
              {b, 1, bMax}
            ]
          ][[2]];
          If[bucket === {},
            {},
            First[bucket]
          ]
        ]
      ],
      Range[2, cMax],
      Method -> "FinestGrained"
    ]
  ];
  squares = Union[Flatten[perC]];
  Total[squares]
];

solve[]
