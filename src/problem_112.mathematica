(* Project Euler Problem 112 — Bouncy Numbers
   https://projecteuler.net/problem=112

   A positive integer is called "increasing" when its decimal digits never decrease from left
   to right (e.g. 134468), and "decreasing" when they never increase (e.g. 66420). Any number
   that is neither increasing nor decreasing is termed "bouncy" (e.g. 155349). No number below
   100 is bouncy. The task asks for the least positive integer n at which the proportion of
   bouncy numbers among {1, ..., n} first reaches exactly 99 percent.

   Mathematical reformulation. Let b(n) count bouncy numbers in {1, ..., n}. We seek the least
   n satisfying 100 b(n) >= 99 n. For a number with digit vector (d_1, ..., d_k), form the
   consecutive differences delta_i = d_{i+1} - d_i for i = 1, ..., k-1. The number is
   increasing iff every delta_i >= 0, decreasing iff every delta_i <= 0, and bouncy iff there
   exist indices with delta_i > 0 and delta_j < 0. Numbers with fewer than three digits
   automatically satisfy one of the two monotonicity conditions, so they are never bouncy.

   Asymptotic analysis. Among all d-digit numbers with d >= 3, the count of non-bouncy numbers
   grows only polynomially in d — specifically C(d+8,9) - 1 increasing numbers and
   C(d+9,9) - C(d,1) decreasing numbers (via stars-and-bars), minus repdigits counted in both
   via inclusion-exclusion. So the fraction of non-bouncy numbers decays rapidly with digit
   length: by d = 7 (numbers above 10^6), the vast majority are bouncy. Empirically the 99%
   threshold is reached near n ~ 1.59 * 10^6, so scanning up to N = 2 * 10^6 is safe. The
   total work is O(N log_{10} N) ~ 1.4 * 10^7 digit-level operations, trivially feasible for
   modern hardware.

   Parallelization strategy. The interval {1, ..., N} is split into approximately 4 * nCores
   contiguous chunks of equal size. Each subkernel independently counts bouncy numbers in its
   assigned chunk — an embarrassingly parallel decomposition, since the bounciness predicate
   depends only on the number itself. Chunk counts are transmitted back to the master kernel
   (one integer per chunk, negligible communication) and prefix-summed via Accumulate. The
   first chunk endpoint where the cumulative bouncy proportion reaches or exceeds 99% is
   located; then a short sequential Do/Catch/Throw scan starting one chunk earlier pinpoints
   the exact threshold crossing. This two-phase design keeps the dominant O(N log N) work
   fully parallel while confining the sequential tail to at most one chunk width.

   Implementation. IntegerDigits extracts digits; Differences computes consecutive deltas; Max
   and Min of the delta list characterise monotonicity in constant time per number. All
   arithmetic is exact integer arithmetic — the condition 100 * b >= 99 * k avoids any
   floating-point division. LaunchKernels[] initialises up to $ProcessorCount subkernels.
   ParallelTable with a Do loop inside each chunk keeps per-kernel allocation minimal. No
   shared mutable state, randomness, or external I/O is used, so the computation is fully
   deterministic and reproducible. *)

nCores = $ProcessorCount;
LaunchKernels[];

solve[] := Module[
  {upperBound = 2000000, chunkSize, nChunks, chunkCounts, cumCounts,
   targetChunk, bouncyCount, searchStart},
  chunkSize = Max[10000, Ceiling[upperBound / (nCores * 4)]];
  nChunks = Ceiling[upperBound / chunkSize];
  chunkCounts = ParallelTable[
    Module[{lo = (i - 1) * chunkSize + 1,
            hi = Min[i * chunkSize, upperBound], cnt = 0, d, df},
      Do[
        d = IntegerDigits[j];
        If[Length[d] >= 3,
          df = Differences[d];
          If[Max[df] > 0 && Min[df] < 0, cnt++]
        ],
        {j, lo, hi}
      ];
      cnt
    ],
    {i, nChunks}
  ];
  cumCounts = Accumulate[chunkCounts];
  targetChunk = nChunks;
  Do[
    If[100 * cumCounts[[i]] >= 99 * Min[i * chunkSize, upperBound],
      targetChunk = i; Break[]
    ],
    {i, nChunks}
  ];
  targetChunk = Max[1, targetChunk - 1];
  bouncyCount = If[targetChunk > 1, cumCounts[[targetChunk - 1]], 0];
  searchStart = (targetChunk - 1) * chunkSize;
  Catch[
    Do[
      Module[{d = IntegerDigits[k], df},
        If[Length[d] >= 3,
          df = Differences[d];
          If[Max[df] > 0 && Min[df] < 0, bouncyCount++]
        ];
        If[100 * bouncyCount >= 99 * k, Throw[k]]
      ],
      {k, searchStart + 1, upperBound}
    ]
  ]
]

solve[]