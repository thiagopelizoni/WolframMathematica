(* Project Euler 152: https://projecteuler.net/problem=152

  Count the subsets S of distinct integers chosen from 2..80 such that Sum[1/n^2, n in S] equals exactly 1/2.
  The sample decomposition in the statement is only illustrative; the task is the exact cardinality for the full bound 80.

  Direct subset enumeration is infeasible because 79 candidates would yield 2^79 combinations. The key reduction uses
  p-adic layers for odd primes. Fix an odd prime p and let e be the largest p-adic valuation among current candidates.
  For terms with valuation e, write n = p^e m with p not dividing m. After multiplying the target equation by p^(2 e),
  those maximal-valuation terms become p-adic units m^(-2), while every other summand and the right-hand side are
  divisible by p^2. Therefore the chosen maximal-valuation terms must satisfy Sum[m^(-2)] == 0 (mod p^2). This gives a
  necessary congruence on a very small layer. If a number never appears in any layer subset satisfying that congruence,
  it cannot appear in any global solution and may be removed. Repeating over all odd primes to a fixed point preserves
  correctness and shrinks the domain dramatically.

  After pruning, the problem becomes an exact integer subset-sum. Let L be lcm of surviving denominators and set
  a_n = L^2/n^2, T = L^2/2. Then Sum[1/n^2] = 1/2 is equivalent to Sum[a_n] = T over subsets. With m survivors, a
  meet-in-the-middle split gives two lists of size about m/2, so subset-sum generation costs O(2^(m/2)) memory/time and
  matching costs O(2^(m/2)) average-time via hash counting. Here m is small enough that this is comfortably feasible.

  Parallel work is applied to the dominant subset-sum generation stage. For each half, a short prefix is fixed and each
  prefix mask is assigned to one kernel; that kernel computes all tail subset sums locally and shifts them by the prefix
  base sum. Prefix blocks are independent and aggregated by Join, so no shared mutable state is needed. The final match
  against the right-half hash table is a deterministic lookup reduction in the main kernel.

  The Wolfram Language implementation keeps all arithmetic exact integers/rationals, uses IntegerExponent and PowerMod
  for p-adic congruence filters, Complement/Pick for iterative elimination, Fold-based subset-sum expansion, and
  Counts/Lookup for deterministic pair aggregation. A bounded launch attempt targets up to $ProcessorCount - 1
  subkernels, and ParallelMap is used on the subset-sum stage whenever kernels are available. *)

nCores = $ProcessorCount;

ClearAll[subsetSupportFlags, reduceCandidates, parallelSubsetSums, solve];

subsetSupportFlags[residues_List, mod_Integer] := Module[
  {k, bitRange, support, bits},
  k = Length[residues];
  bitRange = Range[0, k - 1];
  support = ConstantArray[False, k];
  Do[
    bits = BitGet[mask, bitRange];
    If[
      Mod[residues.bits, mod] == 0,
      support = MapThread[Or, {support, Thread[bits == 1]}]
    ],
    {mask, 0, 2^k - 1}
  ];
  support
];

reduceCandidates[nMax_Integer] := Module[
  {candidates, oddPrimes, changed, exponents, e, high, mod, residues, keepFlags, remove},
  candidates = Range[2, nMax];
  oddPrimes = Rest[Prime[Range[PrimePi[nMax]]]];
  changed = True;
  While[changed,
    changed = False;
    Do[
      exponents = IntegerExponent[candidates, p];
      e = Max[exponents];
      If[e > 0,
        high = Pick[candidates, exponents, e];
        mod = p^2;
        residues = PowerMod[Quotient[high, p^e], -2, mod];
        keepFlags = subsetSupportFlags[residues, mod];
        remove = Pick[high, Not /@ keepFlags];
        If[remove =!= {},
          candidates = Complement[candidates, remove];
          changed = True
        ]
      ],
      {p, oddPrimes}
    ]
  ];
  candidates
];

parallelSubsetSums[values_List, workers_Integer] := Module[
  {m, p, prefixValues, tailValues, bitRange, blocks},
  m = Length[values];
  If[m == 0,
    Return[{0}]
  ];
  If[workers <= 1,
    Return[Fold[Join[#1, #1 + #2] &, {0}, values]]
  ];
  p = Min[m, Max[1, Ceiling[Log2[workers]]]];
  prefixValues = Take[values, p];
  tailValues = Drop[values, p];
  bitRange = Range[0, p - 1];
  blocks = ParallelMap[
    Function[mask,
      Module[{bits, base, tailSums},
        bits = BitGet[mask, bitRange];
        base = prefixValues.bits;
        tailSums = Fold[Join[#1, #1 + #2] &, {0}, tailValues];
        base + tailSums
      ]
    ],
    Range[0, 2^p - 1],
    Method -> "CoarsestGrained"
  ];
  Join @@ blocks
];

solve[] := Module[
  {nMax = 80, kernelsTarget, workers, candidates, l, l2, values, half, leftValues, rightValues,
   leftSums, rightSums, rightCounts, target},
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
  candidates = reduceCandidates[nMax];
  l = LCM @@ candidates;
  l2 = l^2;
  values = Quotient[l2, #^2] & /@ candidates;
  target = Quotient[l2, 2];
  half = Ceiling[Length[values]/2];
  leftValues = Take[values, half];
  rightValues = Drop[values, half];
  leftSums = parallelSubsetSums[leftValues, workers];
  rightSums = parallelSubsetSums[rightValues, workers];
  rightCounts = Counts[rightSums];
  Total[Lookup[rightCounts, target - #, 0] & /@ leftSums]
];

solve[]
