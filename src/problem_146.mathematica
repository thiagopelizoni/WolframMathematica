(* Project Euler 146: https://projecteuler.net/problem=146

  We seek all n < 150000000 such that n^2 + 1, 3, 7, 9, 13, 27 are prime and are consecutive within that interval,
  so every other odd offset between 1 and 27 is composite. The required composite checks reduce to 11, 17, 19, 21, 23,
  because for admissible n we enforce n divisible by 10, which already makes offsets 5, 15, 25 divisible by 5.

  The search is controlled by congruences. If n^2 + a is prime for all required offsets a, then n cannot lie in residue
  classes where n^2 + a vanishes modulo small primes. A wheel modulus 2*3*5*7*11*13 = 30030 removes most impossible n
  immediately and enforces n = 0 (mod 10). Additional modular sieving with primes 17..97 discards further classes before
  any expensive primality test. This is a standard local obstruction sieve for prime constellations over polynomial values.

  After sieving, only a tiny subset survives, and exact primality checks are applied to the six required offsets and to
  the five mandatory composite offsets. The arithmetic is entirely integer and deterministic; no probabilistic shortcut is
  used beyond Wolfram's certified PrimeQ behavior for machine-size integers.

  Let L = 150000000, M = 30030, and R be the admissible wheel residues. The main loop inspects roughly L/M per residue,
  then a constant-size modular filter, so the sieve stage is near O(|R| * L/M). The primality stage is O(S) tests where S
  is the surviving count after modular elimination, empirically several orders smaller than L. This is well within Project
  Euler feasibility bounds.

  Parallelization is embarrassingly parallel over wheel residues. Each residue class defines an independent arithmetic
  progression n = r + kM, with local sieve and local primality verification. Kernels process disjoint residue classes under
  dynamic scheduling via ParallelMap; each kernel returns a partial sum, and the final answer is their Total, an associative
  reduction without shared mutable state.

  The Wolfram Language implementation uses Associations for modular bad-residue lookup, exact integer expressions for n^2,
  and pure functional selection pipelines. All parameters are fixed in-file, yielding reproducible output in a fresh kernel. *)

nCores = $ProcessorCount;

ClearAll[buildWheelResidues, buildBadResidues, preFilterQ, validNQ, residueContribution, solve];

buildWheelResidues[wheelMod_Integer, reqOffsets_List] := Select[
  Range[0, wheelMod - 1],
  Function[r,
    Mod[r, 10] == 0 && AllTrue[reqOffsets, CoprimeQ[r r + #, wheelMod] &]
  ]
];

buildBadResidues[filterPrimes_List, reqOffsets_List] := Association@Table[
  p -> With[
    {
      bad = Select[
        Range[0, p - 1],
        Function[r, AnyTrue[reqOffsets, Mod[r r + #, p] == 0 &]]
      ]
    },
    AssociationThread[bad, ConstantArray[True, Length[bad]]]
  ],
  {p, filterPrimes}
];

preFilterQ[n_Integer, filterPrimes_List, badResidues_Association] := AllTrue[
  filterPrimes,
  Function[p, ! TrueQ[Lookup[badResidues[p], Mod[n, p], False]]]
];

validNQ[n_Integer, reqOffsets_List, compOffsets_List] := Module[
  {n2},
  n2 = n n;
  AllTrue[reqOffsets, PrimeQ[n2 + #] &] && AllTrue[compOffsets, Not@PrimeQ[n2 + #] &]
];

residueContribution[
  r_Integer,
  limit_Integer,
  wheelMod_Integer,
  filterPrimes_List,
  badResidues_Association,
  reqOffsets_List,
  compOffsets_List
] := Module[
  {start, seq, candidates},
  start = If[r == 0, wheelMod, r];
  If[start > limit,
    0,
    seq = Range[start, limit, wheelMod];
    candidates = Select[seq, preFilterQ[#, filterPrimes, badResidues] &];
    Total[Select[candidates, validNQ[#, reqOffsets, compOffsets] &]]
  ]
];

solve[] := Module[
  {
    limit, reqOffsets, compOffsets, wheelPrimes, wheelMod, wheelResidues,
    filterPrimes, badResidues, partialSums
  },
  limit = 150000000;
  reqOffsets = {1, 3, 7, 9, 13, 27};
  compOffsets = {11, 17, 19, 21, 23};
  wheelPrimes = {2, 3, 5, 7, 11, 13};
  wheelMod = Times @@ wheelPrimes;
  wheelResidues = buildWheelResidues[wheelMod, reqOffsets];
  filterPrimes = Select[Prime[Range[PrimePi[97]]], # > Max[wheelPrimes] &];
  badResidues = buildBadResidues[filterPrimes, reqOffsets];
  If[$KernelCount < nCores, LaunchKernels[nCores - $KernelCount]];
  DistributeDefinitions[
    preFilterQ,
    validNQ,
    residueContribution,
    limit,
    wheelMod,
    filterPrimes,
    badResidues,
    reqOffsets,
    compOffsets
  ];
  partialSums = ParallelMap[
    residueContribution[#, limit, wheelMod, filterPrimes, badResidues, reqOffsets, compOffsets] &,
    wheelResidues,
    Method -> "FinestGrained"
  ];
  Total[partialSums]
];

solve[]
