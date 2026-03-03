(* Project Euler 163: https://projecteuler.net/problem=163

The object is a size-n equilateral triangle assembled from n^2 size-1 blocks.
Each size-1 block has the three cevians from vertices to opposite side midpoints.
Let T(n) be the number of triangles of any orientation, shape, size, and position induced by the full cross-hatched segment arrangement. Given T(1)=16 and
T(2)=104, we must compute T(36).

A direct combinatorial classification by triangle type is cumbersome; instead we view the drawing as a finite arrangement of straight lines.
Every admissible side is collinear with one of six global directions (0, 30, 60, 90, 120, 150 degrees), and every triangle is determined by an
unordered triple of non-parallel lines whose
pairwise intersections are distinct and inside the outer hull. For size n the arrangement has 9n-3 lines, so for n=36 there are 321 lines and C(321,3)=5,461,280
triples, already feasible with pruning.

To avoid floating error from sqrt(3), we apply an exact linear coordinate change that maps all relevant vertices and midpoints to Z^2. Lines are represented by
integer coefficients (a,b,c) in ax+by=c. Pair intersections are stored as integer triples (xNum,yNum,det) corresponding to rational coordinates
(xNum/det,yNum/det), and hull-membership is tested by sign-consistent determinant inequalities without forming inexact numbers.

Complexity is dominated by triple enumeration. We precompute all O(m^2) pair intersections and inside-hull flags (m=9n-3), then enumerate triples in
O(m^3) worst case with strong pruning: any pair not intersecting inside the hull is discarded before scanning the third line. For n=36 this is comfortably small in
memory and runtime in Wolfram Language.

Parallelization is over the first line index i. Each worker processes disjoint ranges of i and counts valid triangles with i<j<k using only read-only precomputed
pair tables. Partial counts are reduced by Total, an associative deterministic aggregation, so no shared mutable state or race condition appears. The code uses
ParallelTable with serial fallback for environments where subkernels are unavailable.

Implementation uses exact integer arithmetic throughout, compact list-based line storage, deterministic nested iteration for pruning, and a
single solve[] entry point
returning the required integer T(36). *)

nCores = $ProcessorCount;

ClearAll[
  lineFromPoints,
  intersectData,
  insideHullData,
  samePointData,
  buildLines,
  countForIndex,
  solve
];

lineFromPoints[p1_List, p2_List] := {
  p1[[2]] - p2[[2]],
  p2[[1]] - p1[[1]],
  p2[[1]]*p1[[2]] - p1[[1]]*p2[[2]]
};

intersectData[l1_List, l2_List] := Module[
  {det, xNum, yNum},
  det = l1[[1]]*l2[[2]] - l2[[1]]*l1[[2]];
  If[
    det == 0,
    Return[None]
  ];
  xNum = l1[[3]]*l2[[2]] - l2[[3]]*l1[[2]];
  yNum = l1[[1]]*l2[[3]] - l2[[1]]*l1[[3]];
  {xNum, yNum, det}
];

insideHullData[p_List, hullLines_List] := Module[
  {xNum, yNum, det, s, n1, n2, n3},
  {xNum, yNum, det} = p;
  s = Sign[det];
  n1 = hullLines[[1, 1]]*xNum + hullLines[[1, 2]]*yNum - hullLines[[1, 3]]*det;
  If[
    s*n1 < 0,
    Return[False]
  ];
  n2 = hullLines[[2, 1]]*xNum + hullLines[[2, 2]]*yNum - hullLines[[2, 3]]*det;
  If[
    s*n2 < 0,
    Return[False]
  ];
  n3 = hullLines[[3, 1]]*xNum + hullLines[[3, 2]]*yNum - hullLines[[3, 3]]*det;
  s*n3 >= 0
];

samePointData[p1_List, p2_List] :=
  p1[[1]]*p2[[3]] == p2[[1]]*p1[[3]] && p1[[2]]*p2[[3]] == p2[[2]]*p1[[3]];

buildLines[n_Integer?Positive] := Module[
  {
    familyAB,
    familyABC,
    familyAC,
    familyBC,
    familyBAC,
    familyCAB
  },
  familyAB = Table[
    lineFromPoints[{0, 2*i}, {4, 2*i}],
    {i, 0, n - 1}
  ];
  familyABC = Join[
    Table[
      lineFromPoints[{4*i, 0}, {4*i + 3, 1}],
      {i, 0, n - 1}
    ],
    Table[
      lineFromPoints[{-4*i, 0}, {3 - 4*i, 1}],
      {i, 1, n - 1}
    ]
  ];
  familyAC = Table[
    lineFromPoints[{4*i, 0}, {4*i + 2, 2}],
    {i, 0, n - 1}
  ];
  familyBC = Table[
    lineFromPoints[{4*i + 4, 0}, {4*i + 2, 2}],
    {i, 0, n - 1}
  ];
  familyBAC = Table[
    lineFromPoints[{4*i + 4, 0}, {4*i + 1, 1}],
    {i, 0, 2*n - 2}
  ];
  familyCAB = Table[
    lineFromPoints[{2*i, 0}, {2*i, 2}],
    {i, 1, 2*n - 1}
  ];
  Join[familyAB, familyABC, familyAC, familyBC, familyBAC, familyCAB]
];

countForIndex[
  i_Integer?Positive,
  pairData_List,
  pairInside_List,
  m_Integer?Positive
] := Module[
  {j, k, p12, p23, total = 0},
  For[j = i + 1, j <= m - 1, j++,
    If[
      !pairInside[[i, j]],
      Continue[]
    ];
    p12 = pairData[[i, j]];
    For[k = j + 1, k <= m, k++,
      If[
        !pairInside[[i, k]] || !pairInside[[j, k]],
        Continue[]
      ];
      p23 = pairData[[j, k]];
      If[
        samePointData[p12, p23],
        Continue[]
      ];
      total++;
    ];
  ];
  total
];

solve[] := Module[
  {
    n = 36,
    lines,
    m,
    a,
    b,
    c,
    hullLines,
    pairData,
    pairInside,
    i,
    j,
    p,
    workers,
    partialCounts
  },
  lines = buildLines[n];
  m = Length[lines];
  a = {0, 0};
  b = {4*n, 0};
  c = {2*n, 2*n};
  hullLines = {
    lineFromPoints[a, b],
    lineFromPoints[b, c],
    lineFromPoints[c, a]
  };
  pairData = Table[None, {m}, {m}];
  pairInside = Table[False, {m}, {m}];
  For[i = 1, i <= m - 1, i++,
    For[j = i + 1, j <= m, j++,
      p = intersectData[lines[[i]], lines[[j]]];
      pairData[[i, j]] = p;
      If[
        p =!= None,
        pairInside[[i, j]] = insideHullData[p, hullLines]
      ];
    ];
  ];
  workers = Max[1, nCores - 1];
  partialCounts = If[
    workers > 1,
    Block[
      {$Messages = {}},
      Check[
        ParallelTable[
          countForIndex[i, pairData, pairInside, m],
          {i, 1, m - 2},
          Method -> "CoarsestGrained"
        ],
        Table[
          countForIndex[i, pairData, pairInside, m],
          {i, 1, m - 2}
        ]
      ]
    ],
    Table[
      countForIndex[i, pairData, pairInside, m],
      {i, 1, m - 2}
    ]
  ];
  Total[partialCounts]
];

solve[]
