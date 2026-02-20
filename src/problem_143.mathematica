(* Project Euler 143: https://projecteuler.net/problem=143

  We must evaluate the sum of all distinct integers n <= 120000 for which there exist positive integers p, q, r
  such that n = p + q + r and each of p^2 + pq + q^2, q^2 + qr + r^2, r^2 + rp + p^2 is a perfect square.
  Geometrically this is the arithmetic form of integer Torricelli triangles, but computationally it is a graph
  problem on the symmetric binary relation E(x, y): x^2 + xy + y^2 is a square.

  The key reduction is to generate E without scanning all O(L^2) pairs. The Eisenstein-type equation
  x^2 + xy + y^2 = z^2 has the complete primitive parametrization
  x = m^2 - n^2, y = 2mn + n^2, z = m^2 + mn + n^2 with m > n >= 1, gcd(m, n) = 1, and m - n not divisible by 3;
  all non-primitive solutions are scalar multiples. After sorting x, y, every admissible unordered pair appears
  exactly from this primitive family plus scaling. Because x + y = m(m + 2n), only parameters with m(m + 2n) <= L
  contribute to sums up to L.

  Let L = 120000. We build all edges (a, b) with a < b and a + b <= L from the parametrization, deduplicate, then
  orient edges upward by value. A valid triple is exactly a 3-cycle p < q < r with edges (p, q), (q, r), (p, r).
  Enumerating by smallest vertex p gives wedge checks from q in N+(p) to r in N+(q), with constant-time membership
  test r in N+(p). Every cycle is seen once, so no combinatorial overcount of triples occurs; only final n-values are
  deduplicated before summation to enforce the distinct-sum condition in the statement.

  The generation cost is proportional to the number of produced edges, asymptotically near O(L log L) for this
  divisor-like scaling family, and empirically small at L = 120000. The cycle phase is O(sum_q d-(q)d+(q)), which is
  sparse and far below quadratic here. Memory is linear in edge count. These bounds are comfortably feasible.

  Parallelization is embarrassingly clean in two independent stages. First, the m-parameter range is partitioned
  dynamically by ParallelTable so each kernel emits local edge blocks, then the master kernel merges and deduplicates.
  Second, the set of starting vertices p is chunked into many more blocks than cores; ParallelMap schedules these
  chunks dynamically, each worker computes a private local set of candidate sums, and aggregation is associative:
  flatten, deduplicate, total. No shared mutable state is needed, so race conditions are excluded by construction.

  The Wolfram Language implementation uses exact integer arithmetic only. Associations represent adjacency lists and
  hash-style neighbor sets for O(1) expected membership tests. GroupBy, DeleteDuplicates, Flatten, ParallelTable, and
  ParallelMap provide compact high-level kernels while preserving deterministic output from a pure reduction pipeline. *)

nCores = $ProcessorCount;

ClearAll[chunkList, generatePairsForM, sumsFromChunk, solve];

chunkList[list_List, parts_Integer] := Module[
  {n, size},
  n = Length[list];
  If[n == 0,
    {},
    size = Max[1, Quotient[n + parts - 1, parts]];
    Partition[list, UpTo[size]]
  ]
];

generatePairsForM[m_Integer, limit_Integer] := Module[
  {nMax},
  nMax = Min[m - 1, Quotient[Quotient[limit, m] - m, 2]];
  If[nMax < 1,
    {},
    Flatten[
      Table[
        If[CoprimeQ[m, n] && Mod[m - n, 3] != 0,
          Module[{u0, v0, s0, dMax},
            u0 = m m - n n;
            v0 = 2 m n + n n;
            If[u0 > v0, {u0, v0} = {v0, u0}];
            s0 = u0 + v0;
            dMax = Quotient[limit, s0];
            Table[{d u0, d v0}, {d, 1, dMax}]
          ],
          Nothing
        ],
        {n, 1, nMax}
      ],
      1
    ]
  ]
];

sumsFromChunk[chunk_List, adjAssoc_Association, limit_Integer] := Module[
  {local, p, pNeighbors, pSet, q, qNeighbors, r, s},
  local = <||>;
  Do[
    pNeighbors = Lookup[adjAssoc, p, {}];
    If[pNeighbors =!= {},
      pSet = AssociationThread[pNeighbors, ConstantArray[True, Length[pNeighbors]]];
      Do[
        qNeighbors = Lookup[adjAssoc, q, {}];
        Do[
          If[TrueQ[Lookup[pSet, r, False]],
            s = p + q + r;
            If[s <= limit, local[s] = True]
          ],
          {r, qNeighbors}
        ],
        {q, pNeighbors}
      ]
    ],
    {p, chunk}
  ];
  Keys[local]
];

solve[] := Module[
  {limit, mMax, edgeParts, edges, adjAssoc, pKeys, nChunks, pChunks, partialSums},
  limit = 120000;
  If[$KernelCount < nCores, LaunchKernels[nCores - $KernelCount]];
  mMax = Floor[Sqrt[limit]];
  DistributeDefinitions[generatePairsForM, limit];
  edgeParts = ParallelTable[generatePairsForM[m, limit], {m, 2, mMax}];
  edges = Sort[DeleteDuplicates[Flatten[edgeParts, 1]]];
  adjAssoc = Association[GroupBy[edges, First -> Last, Sort[DeleteDuplicates[#]] &]];
  pKeys = Keys[adjAssoc];
  nChunks = Max[1, 8 nCores];
  pChunks = chunkList[pKeys, nChunks];
  DistributeDefinitions[adjAssoc, limit];
  partialSums = ParallelMap[sumsFromChunk[#, adjAssoc, limit] &, pChunks];
  Total[DeleteDuplicates[Flatten[partialSums, 1]]]
];

solve[]
