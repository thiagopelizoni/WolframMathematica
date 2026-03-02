(* Project Euler 161: https://projecteuler.net/problem=161

The task is to count exact tilings of a 9x12 rectangle with triominoes: the straight 1x3 piece in both
orientations and the L piece in all four quarter-turn orientations. Every covering must be perfect.

A full board search is combinatorially explosive, so we use a transfer formulation on vertical profiles.
Sweeping by columns, any triomino can reach at most two columns ahead, thus the future is fully encoded by
three height-9 occupancy masks (a,b,c): current column, next column, and second-next column. A bit value 1
means the cell is already occupied by tiles started earlier. For each profile we repeatedly place one piece on
the first zero bit of a; when a becomes full we shift to the next profile (b,c,0). This canonical first-gap
construction enforces a unique placement order, so each tiling is counted once.

For fixed height h=9, local branching is bounded: each recursion level tests at most six triomino
orientations and depth is at most h. Therefore one-state transition generation is O(1) in the fixed-height
regime. Let S be the reachable profile count; the width-w dynamic program is O(w S) time and O(S) memory.
Empirically S is only a few thousand for h=9, making exact integer arithmetic entirely practical.

Parallel work decomposition is layer-wise and embarrassingly parallel. At each width step, every active state
contributes independently to successor states using a precomputed transition table. We partition active states
across kernels with ParallelMap and aggregate partial associations by Merge[..., Total], which is associative
and deterministic. This eliminates race conditions and yields reproducible integer totals.

In Wolfram Language, profiles are compact integers manipulated by BitAnd, BitOr, BitShiftRight, and shifts.
Row-dependent legal moves are precompiled as bitmask triples, transition recursion is memoized, and the set of
reachable states is first closed under the transition relation. The final solve[] call performs the parallel
layer DP and returns the exact Project Euler answer. *)

nCores = $ProcessorCount;

ClearAll[
  launchWorkers,
  firstZeroBit,
  solve
];

launchWorkers[target_] := Module[
  {current},
  current = Length[Kernels[]];
  Max[1, Min[target, current]]
];

firstZeroBit[mask_, height_] := Module[
  {r = 0},
  While[
    BitGet[mask, r] == 1,
    r++
  ];
  r
];

solve[] := Module[
  {
    rows = 9,
    cols = 12,
    height,
    width,
    fullMask,
    movesByRow,
    transitionsMasks,
    transitionMap,
    seen,
    queue,
    qIndex,
    state,
    tr,
    transitionTable,
    weightedTransitions,
    workersTarget,
    workers,
    curr,
    entries,
    pieces
  },
  height = Min[rows, cols];
  width = Max[rows, cols];
  If[
    Mod[height*width, 3] != 0,
    Return[0]
  ];
  fullMask = BitShiftLeft[1, height] - 1;
  movesByRow = Table[
    Module[
      {moves = {}},
      If[
        r + 2 < height,
        moves = Append[
          moves,
          {
            BitShiftLeft[1, r] + BitShiftLeft[1, r + 1] + BitShiftLeft[1, r + 2],
            0,
            0
          }
        ]
      ];
      moves = Append[
        moves,
        {
          BitShiftLeft[1, r],
          BitShiftLeft[1, r],
          BitShiftLeft[1, r]
        }
      ];
      If[
        r + 1 < height,
        moves = Append[
          moves,
          {
            BitShiftLeft[1, r] + BitShiftLeft[1, r + 1],
            BitShiftLeft[1, r],
            0
          }
        ]
      ];
      If[
        r + 1 < height,
        moves = Append[
          moves,
          {
            BitShiftLeft[1, r] + BitShiftLeft[1, r + 1],
            BitShiftLeft[1, r + 1],
            0
          }
        ]
      ];
      If[
        r + 1 < height,
        moves = Append[
          moves,
          {
            BitShiftLeft[1, r],
            BitShiftLeft[1, r] + BitShiftLeft[1, r + 1],
            0
          }
        ]
      ];
      If[
        r > 0,
        moves = Append[
          moves,
          {
            BitShiftLeft[1, r],
            BitShiftLeft[1, r] + BitShiftLeft[1, r - 1],
            0
          }
        ]
      ];
      moves
    ],
    {r, 0, height - 1}
  ];
  transitionsMasks[a_, b_, c_] := transitionsMasks[a, b, c] = Module[
    {r, res = <||>, na, nb, nc, sub},
    If[
      a == fullMask,
      Return[<|b + BitShiftLeft[c, height] -> 1|>]
    ];
    r = firstZeroBit[a, height];
    Do[
      If[
        BitAnd[a, move[[1]]] == 0 && BitAnd[b, move[[2]]] == 0 && BitAnd[c, move[[3]]] == 0,
        na = BitOr[a, move[[1]]];
        nb = BitOr[b, move[[2]]];
        nc = BitOr[c, move[[3]]];
        sub = transitionsMasks[na, nb, nc];
        KeyValueMap[
          (res[#1] = Lookup[res, #1, 0] + #2) &,
          sub
        ]
      ],
      {move, movesByRow[[r + 1]]}
    ];
    res
  ];
  transitionMap[s_] := transitionMap[s] = Module[
    {a, b, c},
    a = BitAnd[s, fullMask];
    b = BitAnd[BitShiftRight[s, height], fullMask];
    c = BitShiftRight[s, 2*height];
    transitionsMasks[a, b, c]
  ];
  seen = <|0 -> True|>;
  queue = {0};
  qIndex = 1;
  While[
    qIndex <= Length[queue],
    state = queue[[qIndex]];
    qIndex++;
    tr = transitionMap[state];
    KeyValueMap[
      If[
        !KeyExistsQ[seen, #1],
        seen[#1] = True;
        queue = Append[queue, #1]
      ] &,
      tr
    ]
  ];
  transitionTable = AssociationMap[transitionMap, Keys[seen]];
  weightedTransitions[entry_] := Module[
    {out = <||>, ways, localTransitions},
    ways = entry[[2]];
    localTransitions = transitionTable[entry[[1]]];
    KeyValueMap[
      (out[#1] = ways*#2) &,
      localTransitions
    ];
    out
  ];
  workersTarget = Max[0, nCores - 1];
  workers = launchWorkers[workersTarget];
  If[
    workers > 1,
    DistributeDefinitions[transitionTable, weightedTransitions]
  ];
  curr = <|0 -> 1|>;
  Do[
    entries = Normal[curr];
    pieces = If[
      workers > 1 && Length[entries] > 1,
      Block[
        {$Messages = {}},
        Check[
          ParallelMap[
            weightedTransitions,
            entries,
            Method -> "CoarsestGrained"
          ],
          Map[
            weightedTransitions,
            entries
          ]
        ]
      ],
      Map[
        weightedTransitions,
        entries
      ]
    ];
    curr = Merge[pieces, Total],
    {width}
  ];
  Lookup[curr, 0, 0]
];

solve[]
