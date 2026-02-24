(* Project Euler 153: https://projecteuler.net/problem=153

  Let s(n) denote the sum, in the Gaussian integers, of all divisors of the rational integer n that have strictly positive real part.
  Because non-real divisors occur in conjugate pairs, each s(n) is an ordinary integer. The objective is to compute
  Sum[s(n), {n, 1, 10^8}] exactly.

  For z = a + b i with a > 0, z divides an integer n exactly when n (a - b i)/(a^2 + b^2) has integral real and imaginary parts,
  which is equivalent to (a^2 + b^2)/g dividing n, where g = gcd(a, b). Hence z contributes Re(z) = a to every multiple of
  (a^2 + b^2)/g, so its total contribution up to N is a Floor[N g/(a^2 + b^2)]. Splitting b = 0 and b != 0 gives
  S(N) = Sum[a Floor[N/a], {a >= 1}] + 2 Sum[a Floor[N gcd(a,b)/(a^2+b^2)], {a,b >= 1}].

  Write a = g x, b = g y with gcd(x, y) = 1. Then the complex part becomes
  2 Sum[x Sum[g Floor[N/(g (x^2+y^2))], {g >= 1}], {x,y >= 1, gcd(x,y)=1}].
  For q = Floor[N/(x^2+y^2)], the inner sum is Sum[g Floor[q/g], {g, 1, q}], which equals the summatory divisor function
  A(q) = Sum[sigma(m), {m, 1, q}]. Therefore each primitive pair contributes 2 x A(Floor[N/(x^2+y^2)]).

  The function A(q) is evaluated by Dirichlet hyperbola grouping of equal quotients:
  A(q) = Sum[k Floor[q/k], {k,1,q}] in O(sqrt(q)) arithmetic operations. Only O(sqrt(N)) distinct quotient values
  Floor[N/t] exist, so memoized precomputation of all needed A-values costs O(N^(3/4)) time and O(sqrt(N)) memory.
  The dominant stage is primitive lattice traversal in the quarter disk x^2 + y^2 <= N, which is O(N) lightweight integer work;
  for N = 10^8 this is computationally feasible.

  Parallelization is performed on disjoint chunks of x-values. Each kernel processes independent x-ranges, scans valid y,
  applies the coprimality filter, and accumulates weighted A-lookups locally. Partial sums are combined by Total, an
  associative and deterministic reduction without shared mutable state.

  In Wolfram Language, exact integer arithmetic is used throughout; quotient grouping is implemented with While and Quotient,
  cached via memoization, primitive-pair filtering uses CoprimeQ, and workload partitioning uses Partition plus ParallelMap.
  Kernels are launched dynamically up to $ProcessorCount - 1 with a bounded attempt, then the script falls back deterministically
  to serial execution if parallel kernels are unavailable. *)

nCores = $ProcessorCount;

ClearAll[
  sigmaPrefix,
  precomputeSigmaValues,
  xContribution,
  chunkContribution,
  solve
];

sigmaPrefix[0] = 0;

sigmaPrefix[n_Integer?Positive] := sigmaPrefix[n] = Module[
  {k = 1, q, kMax, acc = 0},
  While[k <= n,
    q = Quotient[n, k];
    kMax = Quotient[n, q];
    acc += q (k + kMax) (kMax - k + 1)/2;
    k = kMax + 1;
  ];
  acc
];

precomputeSigmaValues[n_Integer?Positive] := Module[
  {r, qValues},
  r = Floor[Sqrt[n]];
  qValues = Union[Join[Range[1, r], Quotient[n, Range[1, r]]]];
  sigmaPrefix /@ qValues;
  Null
];

xContribution[x_Integer, n_Integer] := Module[
  {xx, yMax, y, q, w, acc = 0},
  xx = x*x;
  yMax = Min[x, Floor[Sqrt[n - xx]]];
  For[y = 1, y <= yMax, y++,
    If[CoprimeQ[x, y],
      q = Quotient[n, xx + y*y];
      w = If[y == x, x, x + y];
      acc += 2 w sigmaPrefix[q];
    ];
  ];
  acc
];

chunkContribution[xs_List, n_Integer] := Module[
  {acc = 0},
  Do[
    acc += xContribution[x, n],
    {x, xs}
  ];
  acc
];

solve[n_Integer?Positive] := Module[
  {rootN, rationalPart, kernelsTarget, workers, chunkSize, xChunks, complexPart},
  rootN = Floor[Sqrt[n]];
  precomputeSigmaValues[n];
  rationalPart = sigmaPrefix[n];
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
  If[
    workers > 1,
    DistributeDefinitions[sigmaPrefix, xContribution, chunkContribution];
    chunkSize = Max[1, Ceiling[rootN/(8 workers)]];
    xChunks = Partition[Range[1, rootN], UpTo[chunkSize]];
    complexPart = Total[
      ParallelMap[
        chunkContribution[#, n] &,
        xChunks,
        Method -> "CoarsestGrained"
      ]
    ],
    complexPart = chunkContribution[Range[1, rootN], n]
  ];
  rationalPart + complexPart
];

solve[] := solve[10^8];

solve[]
