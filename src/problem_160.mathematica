(* Project Euler 160: https://projecteuler.net/problem=160

Define f(N) as the last five non-zero decimal digits of N!. The task asks for f(10^12).
Equivalently, if v_p(N!) is the p-adic valuation, write N! = 2^a 5^b U with gcd(U,10)=1 and
b=v_5(N!). Then f(N) is represented modulo 10^5 by 2^(a-b) U.

The computation is best split by CRT with moduli 2^5 and 5^5. For N=10^12, a-b is enormous,
so 2^(a-b) is divisible by 32 and the residue modulo 2^5 is exactly 0. The nontrivial part is
modulo 5^5=3125, where 2 is invertible. Let F(N)=N!/5^{v_5(N!)} mod 3125, i.e. factorial with
all factors of 5 removed. Then f(N) mod 3125 = F(N) * 2^{-v_5(N!)} mod 3125.

To evaluate F(N), decompose by multiples of 5. If A(n)=Product_{1<=k<=n, 5∤k} k mod 3125,
then F(n)=A(n) F(floor(n/5)). Iterating gives F(n)=Product_j A(floor(n/5^j)). Thus depth is
O(log_5 N). The function A has period 3125 because reduction modulo 3125 preserves residue
classes and divisibility by 5. Precompute prefix products P(r)=Product_{1<=k<=r,5∤k} k mod 3125
for r in [0,3125], and block product G=P(3125). Then A(n)=G^{floor(n/3125)} P(n mod 3125).
Hence total arithmetic is O(3125 + log N), decisively feasible for N=10^12.

Parallelization is applied to the independent factors A(floor(n/5^j)) in the product expansion
of F(n). The list of levels floor(n/5^j) is partition-free and embarrassingly parallel; kernels
compute each term independently and partials are aggregated by modular multiplication, an
associative deterministic reduction. The implementation launches up to $ProcessorCount-1
subkernels, falls back safely to serial mode, and keeps all arithmetic exact.

In Wolfram Language, valuations use repeated Quotient, periodic products use PowerMod with a
precomputed prefix table, and CRT lifting combines residues modulo 32 and 3125 into modulo 10^5.
The script is deterministic, self-contained, and returns the exact Project Euler answer from
solve[]. *)

nCores = $ProcessorCount;

ClearAll[
  valuationFactorial,
  buildFiveFreePrefix,
  coprimeProductUpTo,
  fiveFreeFactorialMod,
  crtCombine,
  solve
];

valuationFactorial[n_Integer?NonNegative, p_Integer?Positive] := Module[
  {x = n, s = 0},
  While[
    x > 0,
    x = Quotient[x, p];
    s += x;
  ];
  s
];

buildFiveFreePrefix[mod_Integer?Positive] := Module[
  {prefix, i},
  prefix = ConstantArray[1, mod + 1];
  For[i = 1, i <= mod, i++,
    prefix[[i + 1]] = If[
      Mod[i, 5] == 0,
      prefix[[i]],
      Mod[prefix[[i]]*i, mod]
    ];
  ];
  {Developer`ToPackedArray[prefix], prefix[[mod + 1]]}
];

coprimeProductUpTo[n_Integer?NonNegative, mod_Integer?Positive, prefix_List, blockProduct_Integer] := Module[
  {q, r},
  q = Quotient[n, mod];
  r = Mod[n, mod];
  Mod[PowerMod[blockProduct, q, mod]*prefix[[r + 1]], mod]
];

fiveFreeFactorialMod[
  n_Integer?NonNegative,
  mod_Integer?Positive,
  prefix_List,
  blockProduct_Integer,
  workers_Integer?Positive
] := Module[
  {levels, x = n, factors},
  levels = {};
  While[
    x > 0,
    levels = Append[levels, x];
    x = Quotient[x, 5];
  ];
  factors = If[
    workers > 1,
    ParallelMap[
      coprimeProductUpTo[#, mod, prefix, blockProduct] &,
      levels,
      Method -> "CoarsestGrained"
    ],
    Map[
      coprimeProductUpTo[#, mod, prefix, blockProduct] &,
      levels
    ]
  ];
  Fold[Mod[#1*#2, mod] &, 1, factors]
];

crtCombine[r2_Integer, r5_Integer, m2_Integer?Positive, m5_Integer?Positive] := Module[
  {k},
  k = Mod[(r5 - r2)*PowerMod[m2, -1, m5], m5];
  r2 + m2*k
];

solve[] := Module[
  {
    n = 10^12,
    mod2 = 2^5,
    mod5 = 5^5,
    kernelsTarget,
    workers,
    prefix,
    blockProduct,
    v2,
    v5,
    unitPart5,
    residue5,
    residue2
  },
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
  {prefix, blockProduct} = buildFiveFreePrefix[mod5];
  If[workers > 1, DistributeDefinitions[coprimeProductUpTo, mod5, prefix, blockProduct]];
  unitPart5 = fiveFreeFactorialMod[n, mod5, prefix, blockProduct, workers];
  v2 = valuationFactorial[n, 2];
  v5 = valuationFactorial[n, 5];
  residue5 = Mod[unitPart5*PowerMod[2, -v5, mod5], mod5];
  residue2 = If[v2 - v5 >= 5, 0, Mod[PowerMod[2, v2 - v5, mod2], mod2]];
  crtCombine[residue2, residue5, mod2, mod5]
];

solve[]
