(* Project Euler 151: https://projecteuler.net/problem=151

  A print shop begins Monday with one A1 sheet in an envelope. For each batch, one sheet is chosen uniformly from the
  envelope; if it is larger than A5, it is repeatedly halved until one A5 is produced for printing, while every unused
  half-sheet is returned to the envelope. Since one A1 equals sixteen A5 sheets, exactly sixteen batches occur. We need
  the expected number of times the envelope contains exactly one sheet, excluding the initial Monday state and the final
  single-sheet state immediately before the last batch.

  Encode a state by s = (a2,a3,a4,a5), the counts of A2..A5 currently in the envelope. After the first forced batch from
  A1, the process starts at s0 = (1,1,1,1) with fifteen batches remaining. From a state s, choose index i with probability
  ai/(a2+a3+a4+a5). The transition removes one sheet of size i and adds one sheet of each smaller size produced during
  cutting, giving a deterministic successor T_i(s). This is a finite Markov chain with exact rational transition weights.

  The target expectation is a linear functional of transient distributions. Let D_t be the distribution at the beginning of
  batch t (t = 2..16). The desired value is Sum_{t=2}^{15} P_{D_t}(a2+a3+a4+a5 = 1). Therefore we can propagate D_t exactly
  for fourteen transitions and accumulate singleton mass before each transition. No Monte Carlo approximation is needed.

  Complexity is bounded by the reachable state set. Area conservation 8 a2 + 4 a3 + 2 a4 + a5 = m (m <= 15 after batch 1)
  implies only O(m^3) admissible integer states; the reachable subset here is tiny, so each propagation step expands each
  state to at most four successors. Total cost is O(B |S|) exact rational operations with B = 14 and small |S|, well below
  Project Euler limits. Memory is O(|S|) for the current distribution association.

  Parallelization is applied to the dominant step: per-state successor expansion. States in a fixed distribution are
  independent subproblems, so we map expansion over state-probability pairs with ParallelMap using finest-grained dynamic
  scheduling. Each kernel emits weighted successor rules; these are aggregated by Merge[..., Total], an associative reduction
  that is deterministic and race-free because no shared mutable state is used.

  In Wolfram Language, states are integer lists used as Association keys, probabilities remain exact rationals throughout,
  and transitions are pure integer updates. Kernels are launched up to $ProcessorCount, definitions are distributed once,
  and the final exact expectation is rounded to six decimal places to match the Project Euler submission format. *)

nCores = $ProcessorCount;

ClearAll[
  transitionState,
  expandState,
  advanceDistribution,
  singletonProbability,
  solve
];

transitionState[state_List, i_Integer] := Module[
  {next = state, j},
  next[[i]] -= 1;
  For[j = i + 1, j <= 4, j++,
    next[[j]] += 1;
  ];
  next
];

expandState[state_ -> prob_] := Module[
  {total, indices},
  total = Total[state];
  indices = Flatten[Position[state, _?(# > 0 &)]];
  Table[
    transitionState[state, i] -> prob state[[i]]/total,
    {i, indices}
  ]
];

advanceDistribution[dist_Association] := Module[
  {pairs, pieces},
  pairs = Normal[dist];
  pieces = Flatten[
    If[
      $KernelCount > 0,
      ParallelMap[expandState, pairs, Method -> "FinestGrained"],
      Map[expandState, pairs]
    ],
    1
  ];
  Merge[pieces, Total]
];

singletonProbability[dist_Association] := Total[
  Values[KeySelect[dist, Total[#] == 1 &]]
];

solve[] := Module[
  {dist, expected, step, targetKernels},
  targetKernels = Max[1, nCores - 1];
  If[
    $KernelCount < targetKernels,
    Quiet[
      Check[
        LaunchKernels[targetKernels - $KernelCount],
        Null
      ],
      {KernelObject::timekernels, LinkConnect::linkc}
    ]
  ];
  If[$KernelCount > 0, DistributeDefinitions[transitionState, expandState]];
  dist = <|{1, 1, 1, 1} -> 1|>;
  expected = 0;
  For[step = 1, step <= 14, step++,
    expected += singletonProbability[dist];
    dist = advanceDistribution[dist];
  ];
  Round[10^6 expected]/10.^6
];

solve[]
