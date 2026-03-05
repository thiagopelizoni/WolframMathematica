(* Project Euler 168: https://projecteuler.net/problem=168

Let n be an L-digit integer with L >= 2, written as n = 10 a + d, where d in {1, ..., 9} is the last digit. Right-rotation sends n to
R(n) = d 10^(L - 1) + a. We seek all n with 10 < n < 10^100 such that n divides R(n), and we must return the last five digits of the
sum of all such n.

If R(n) = k n for an integer k, then because R(n) and n have comparable magnitudes, k must lie in {1, ..., 9}. Eliminating a gives
n = d (10^L - 1) / (10 k - 1). Hence every solution is parameterized by a triple (L, k, d) with L in [2, 100], k in [1, 9], d in [1, 9],
subject to one arithmetic condition: 10 k - 1 must divide d (10^L - 1), plus the L-digit constraint 10^(L - 1) <= n < 10^L. This is a
complete characterization, so no search over candidate integers is needed.

The resulting algorithm is finite and tiny: at most 99 * 9 * 9 = 8019 triples, each handled by exact integer divisibility and quotient
operations on at most 100-digit numbers. Time is O(L_max * 9 * 9) arithmetic operations, effectively constant for Project Euler bounds;
memory is O(number of solutions), also tiny. Modular reduction by 10^5 can be applied at aggregation time without altering correctness.

Parallelization is naturally over digit lengths L. Each L defines an independent set of (k, d) checks and produces a disjoint partial list
of valid n values. These tasks have no shared mutable state, so they can run concurrently with ParallelMap. Final accumulation uses Total
and Mod, which are associative and deterministic, avoiding race conditions and preserving reproducibility.

The Wolfram Language implementation uses exact integer arithmetic only: Divisible, Quotient, Table, Flatten, and DeleteMissing build the
per-length solution lists, while a parallel or serial map over L = 2..100 aggregates results. A guarded kernel launch path provides a
portable fallback when subkernels are unavailable. The main entry point solve[] returns the required five-digit suffix as an integer. *)

nCores = $ProcessorCount;

ClearAll[candidatesForLength, solve];

candidatesForLength[len_Integer] := Module[
  {tenPow, lower},
  tenPow = 10^len;
  lower = 10^(len - 1);
  DeleteMissing[
    Flatten[
      Table[
        Module[
          {den, num, n},
          den = 10 k - 1;
          num = d (tenPow - 1);
          If[
            Divisible[num, den],
            n = Quotient[num, den];
            If[
              lower <= n < tenPow && n > 10,
              n,
              Missing["OutOfRange"]
            ],
            Missing["NotInteger"]
          ]
        ],
        {k, 1, 9},
        {d, 1, 9}
      ],
      1
    ]
  ]
];

solve[] := Module[
  {modulus, lengths, launched, perLength, allValues},
  modulus = 10^5;
  lengths = Range[2, 100];
  perLength = If[
    nCores > 1,
    launched = Quiet[Check[Length[LaunchKernels[]], 0]];
    If[
      launched > 0,
      DistributeDefinitions[candidatesForLength];
      ParallelMap[
        candidatesForLength,
        lengths,
        Method -> "CoarsestGrained"
      ],
      Map[
        candidatesForLength,
        lengths
      ]
    ],
    Map[
      candidatesForLength,
      lengths
    ]
  ];
  allValues = Flatten[perLength];
  Mod[Total[Mod[allValues, modulus]], modulus]
];

solve[]
