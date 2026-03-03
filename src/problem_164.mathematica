(* Project Euler 164: https://projecteuler.net/problem=164

We seek the count of 20-digit base-10 integers with nonzero leading digit such that every length-3 contiguous digit block has sum at most 9.
Equivalently, for digits d1...d20 with d1>=1 and 0<=di<=9, enforce di+di+1+di+2<=9 for i=1,...,18, and count all admissible sequences.

The constraint is local of width three, so the natural state is the ordered pair of last two digits. If a sequence currently ends with (a,b),
admissible extensions are exactly c in {0,...,9-a-b}, giving next state (b,c). Thus the problem is counting walks of fixed length on a finite
directed graph of states {(a,b): a,b in {0,...,9}, a+b<=9}. The state count is Sum_{a=0}^9 (10-a)=55, so dynamic programming is immediate.

Let F_r(a,b) denote the number of valid suffixes of length r that can be appended after trailing pair (a,b). Then
F_0(a,b)=1 and F_r(a,b)=Sum_{c=0}^{9-a-b} F_{r-1}(b,c). For each legal leading pair with first digit nonzero, namely
(a,b) with 1<=a<=9 and 0<=b<=9-a, the target contribution is F_18(a,b), because two digits are already fixed. The final answer is
Sum_{a=1}^9 Sum_{b=0}^{9-a} F_18(a,b).

Asymptotically, with length n and decimal base fixed, the recurrence has O(n*|S|*B) arithmetic operations where |S|=55 and B<=10.
For n=20 this is tiny, and exact integer arithmetic is fully safe. No floating-point operations or approximations are needed.

Parallelization uses independence of starting pairs. Each admissible leading pair defines a disjoint subproblem that computes its own memoized
F-values. These tasks share no mutable state, so they are embarrassingly parallel and race-free. Work is distributed with ParallelMap over all
leading pairs, using the detected core count from $ProcessorCount. Partial counts are combined by Total, an associative deterministic reduction.

Implementation in Wolfram Language uses a local memoized function inside each task, exact integer sums, Table/Flatten to build starting pairs,
and a solve[] entry point returning the exact Project Euler value. *)

nCores = $ProcessorCount;

ClearAll[countFromPair, solve];

countFromPair[pair_List, remaining_Integer?NonNegative] := Module[
  {a, b, f},
  {a, b} = pair;
  f[_, _, 0] := 1;
  f[x_Integer, y_Integer, r_Integer?Positive] :=
    f[x, y, r] = Sum[
      f[y, z, r - 1],
      {z, 0, 9 - x - y}
    ];
  f[a, b, remaining]
];

solve[] := Module[
  {digits, remaining, leadingPairs, partialCounts},
  digits = 20;
  remaining = digits - 2;
  leadingPairs = Flatten[
    Table[
      {a, b},
      {a, 1, 9},
      {b, 0, 9 - a}
    ],
    1
  ];
  partialCounts = If[
    nCores > 1,
    Module[{parallelResult},
      DistributeDefinitions[countFromPair, remaining];
      parallelResult = Quiet[
        Check[
          TimeConstrained[
            ParallelMap[
              countFromPair[#, remaining] &,
              leadingPairs,
              Method -> "FinestGrained"
            ],
            30,
            $Failed
          ],
          $Failed
        ]
      ];
      If[
        parallelResult === $Failed,
        Map[
          countFromPair[#, remaining] &,
          leadingPairs
        ],
        parallelResult
      ]
    ],
    Map[
      countFromPair[#, remaining] &,
      leadingPairs
    ]
  ];
  Total[partialCounts]
];

solve[]
