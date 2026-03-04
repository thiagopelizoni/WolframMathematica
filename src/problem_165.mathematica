(* Project Euler 165: https://projecteuler.net/problem=165

The task defines a deterministic pseudorandom stream by s_(n+1)=s_n^2 mod 50515093 with s_0=290797, then t_n=s_n mod 500.
From these values we form 5000 planar segments L_k joining (t_(4k-3),t_(4k-2)) to (t_(4k-1),t_(4k)). We must count distinct
true intersection points: points strictly interior to both segments, excluding touching endpoints and parallel coincidences.

For segments p->p+r and q->q+s, intersection of supporting lines is governed by the 2D determinant den=cross(r,s). If den=0 the
directions are parallel, so there is no unique crossing to count. Otherwise, barycentric parameters are
u=cross(q-p,s)/den and v=cross(q-p,r)/den. The intersection is a true segment intersection exactly when 0<u<1 and 0<v<1.
Using sign-aware integer inequalities avoids floating arithmetic: for den>0 require 0<numU<den and 0<numV<den; for den<0 require
den<numU<0 and den<numV<0. This is exact and immune to rounding artifacts.

Coordinates are rational numbers. To deduplicate globally, every accepted intersection is mapped to a canonical integer key by reducing
x and y fractions independently via gcd and normalizing denominator sign. Equality of geometric points is then tuple equality of these
reduced rationals, so duplicate crossings produced by different segment pairs collapse correctly.

With N=5000 segments, pair testing is O(N^2)=12,497,500 checks, each O(1) integer arithmetic. This scale is feasible in Mathematica,
especially since denominator and numerator magnitudes stay modest (coordinates are bounded by 0..499, and determinants by about 2.5e5).
Memory is controlled by local deduplication per work unit before final global union.

Parallelization splits the first segment index set {1,...,N-1} into disjoint chunks. Each worker processes all pairs (i,j) with i in its
chunk and j>i, producing a local duplicate-free list of canonical intersection keys. No shared mutable state is needed. Aggregation is
deterministic: local lists are joined and reduced with Union, and the final answer is the length of this set.

The Wolfram Language implementation follows this structure: deterministic sequence generation, preprocessing into start-plus-direction
segment records, exact integer intersection kernel, chunked parallel map over independent index blocks, and a single solve[] returning
the required integer T(5000). *)

nCores = $ProcessorCount;

ClearAll[generateSegments, toCanonical, processChunk, solve];

generateSegments[count_Integer?Positive] := Module[
  {s, vals},
  s = 290797;
  vals = Table[
    s = Mod[s*s, 50515093];
    Mod[s, 500],
    {4*count}
  ];
  Map[
    {#[[1]], #[[2]], #[[3]] - #[[1]], #[[4]] - #[[2]]} &,
    Partition[vals, 4]
  ]
];

toCanonical[xNum_Integer, yNum_Integer, den_Integer] := Module[
  {gx, gy, xn, xd, yn, yd},
  gx = GCD[Abs[xNum], Abs[den]];
  gy = GCD[Abs[yNum], Abs[den]];
  xn = Quotient[xNum, gx];
  xd = Quotient[den, gx];
  yn = Quotient[yNum, gy];
  yd = Quotient[den, gy];
  If[
    xd < 0,
    xn = -xn;
    xd = -xd
  ];
  If[
    yd < 0,
    yn = -yn;
    yd = -yd
  ];
  {xn, xd, yn, yd}
];

processChunk[indices_List, segments_List] := Module[
  {
    n,
    i,
    j,
    x1,
    y1,
    rx,
    ry,
    x3,
    y3,
    sx,
    sy,
    den,
    qpx,
    qpy,
    numU,
    numV,
    xNum,
    yNum,
    bag
  },
  n = Length[segments];
  bag = Reap[
    Do[
      {x1, y1, rx, ry} = segments[[i]];
      Do[
        {x3, y3, sx, sy} = segments[[j]];
        den = rx*sy - ry*sx;
        If[
          den == 0,
          Continue[]
        ];
        qpx = x3 - x1;
        qpy = y3 - y1;
        numU = qpx*sy - qpy*sx;
        numV = qpx*ry - qpy*rx;
        If[
          den > 0,
          If[
            numU <= 0 || numU >= den || numV <= 0 || numV >= den,
            Continue[]
          ],
          If[
            numU >= 0 || numU <= den || numV >= 0 || numV <= den,
            Continue[]
          ]
        ];
        xNum = x1*den + rx*numU;
        yNum = y1*den + ry*numU;
        Sow[toCanonical[xNum, yNum, den]],
        {j, i + 1, n}
      ],
      {i, indices}
    ]
  ][[2]];
  If[
    bag === {},
    {},
    DeleteDuplicates[bag[[1]]]
  ]
];

solve[] := Module[
  {
    segmentCount,
    segments,
    nSegments,
    chunkSize,
    chunks,
    partialLists,
    parallelResult
  },
  segmentCount = 5000;
  segments = generateSegments[segmentCount];
  nSegments = Length[segments];
  chunkSize = Max[
    1,
    Ceiling[(nSegments - 1)/(4*Max[1, nCores])]
  ];
  chunks = Partition[
    Range[nSegments - 1],
    UpTo[chunkSize]
  ];
  partialLists = If[
    nCores > 1,
    Module[{},
      DistributeDefinitions[toCanonical, processChunk, segments];
      parallelResult = Quiet[
        Check[
          TimeConstrained[
            ParallelMap[
              processChunk[#, segments] &,
              chunks,
              Method -> "CoarsestGrained"
            ],
            180,
            $Failed
          ],
          $Failed
        ]
      ];
      If[
        parallelResult === $Failed,
        Map[
          processChunk[#, segments] &,
          chunks
        ],
        parallelResult
      ]
    ],
    Map[
      processChunk[#, segments] &,
      chunks
    ]
  ];
  Length[Union[Join @@ partialLists]]
];

solve[]
