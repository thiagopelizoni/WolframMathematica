(*
  Project Euler Problem 84: Monopoly Odds
  URL: https://projecteuler.net/problem=084

  Problem Statement:
  Calculate the steady-state probabilities of landing on each square of a standard Monopoly board using two 4-sided
  dice. The game includes standard rules: Go to Jail (G2J) square, Community Chest (CC), and Chance (CH) cards.
  Rolling three consecutive doubles sends the player directly to Jail. Determine the three most popular squares and
  output their 2-digit codes concatenated as a 6-digit string.

  Mathematical Analysis:
  The game is modeled as a finite Markov Chain. To handle the "three consecutive doubles" rule, the state space is
  expanded to include the count of consecutive doubles. We define a state as (Square, DoubleCount), where Square is
  an integer in [0, 39] and DoubleCount is in [0, 2]. Total states = 40 * 3 = 120.
  
  We construct a 120x120 transition matrix M where M_{ij} represents the probability of transitioning from state i
  to state j in one turn. The transitions incorporate:
  1. The 16 outcomes of two 4-sided dice (probabilities 1/16 each).
  2. Movement logic: square = (square + roll) mod 40.
  3. Doubles logic: if roll is double, increment counter; if counter reaches 3, go to Jail (10) and reset. If not
     double, reset counter.
  4. Special square resolutions:
     - G2J (30) moves immediately to Jail (10).
     - CC (2, 17, 33) draws a card: 1/16 -> Go, 1/16 -> Jail, 14/16 -> Stay.
     - CH (7, 22, 36) draws a card: 10 moving cards (Go, Jail, C1, E3, H2, R1, Next R x2, Next U, Back 3) and 6
       stay cards.
     - Recursion: If CH (36) draws "Back 3", player lands on CC (33), triggering a CC card draw. The probability
       must be distributed accordingly.

  The steady-state probability vector pi is the left eigenvector of M with eigenvalue 1. We solve the linear system
  (M^T - I) * pi^T = 0 to find pi, then marginalize over the DoubleCount dimension to get the probabilities per square.

  Complexity and Feasibility:
  The state space N=120 is small. Constructing M involves ~120 * 16 operations. Solving the linear system is O(N^3).
  Both are computationally trivial (< 1 second).

  Parallelization Strategy:
  We use `ParallelTable` to construct the rows of the transition matrix M. Each core computes the transition vector
  for a subset of the 120 states. The linear algebra step is performed sequentially on the main core.

  Wolfram Language Implementation:
  - Use `Module` to encapsulate all logic and constants.
  - Implement `resolveLogic` using `Which` to avoid `Return` issues in parallel contexts.
  - Use exact rational arithmetic (e.g., 1/16) to ensure precision.
  - Use `NullSpace` to solve the linear system for the steady state.
*)

solve[] := Module[{
  nCores, squares, nStates, diceOutcomes, ccSquares, chSquares, rSquares, uSquares,
  toIndex, resolveLogic, buildStateRow, transitionMat, steadyState, probs, sorted
  },
  
  nCores = $ProcessorCount;
  squares = Range[0, 39];
  nStates = 120;
  
  diceOutcomes = Tuples[Range[4], 2];
  
  ccSquares = {2, 17, 33};
  chSquares = {7, 22, 36};
  rSquares = {5, 15, 25, 35};
  uSquares = {12, 28};
  
  toIndex = Function[{s, d}, s * 3 + d + 1];
  
  resolveLogic = Function[{sq},
    Which[
      sq == 30, {{10, 1}},
      
      MemberQ[ccSquares, sq],
      {{0, 1/16}, {10, 1/16}, {sq, 14/16}},
      
      MemberQ[chSquares, sq],
      Module[{nr, nu, b3, baseMoves, extraMoves},
        nr = SelectFirst[rSquares, # > sq &];
        If[MissingQ[nr], nr = 5];
        
        nu = SelectFirst[uSquares, # > sq &];
        If[MissingQ[nu], nu = 12];
        
        b3 = sq - 3;
        
        baseMoves = {
          {0, 1/16},
          {10, 1/16},
          {11, 1/16},
          {24, 1/16},
          {39, 1/16},
          {5, 1/16},
          {nr, 2/16},
          {nu, 1/16},
          {sq, 6/16}
        };
        
        extraMoves = Map[{#[[1]], #[[2]]/16} &, resolveLogic[b3]];
        
        Join[baseMoves, extraMoves]
      ],
      
      True, {{sq, 1}}
    ]
  ];
  
  buildStateRow = Function[{s, d},
    Module[{row, r1, r2, sumR, nextS, nextD, outcomes, target},
      row = ConstantArray[0, nStates];
      
      Do[
        r1 = roll[[1]]; r2 = roll[[2]];
        sumR = r1 + r2;
        
        If[r1 == r2,
          If[d == 2,
            target = toIndex[10, 0];
            row[[target]] += 1/16;
            Continue[]
          ];
          nextD = d + 1;
        ,
          nextD = 0;
        ];
        
        nextS = Mod[s + sumR, 40];
        outcomes = resolveLogic[nextS];
        
        Do[
          target = toIndex[res[[1]], nextD];
          row[[target]] += res[[2]]/16;
        , {res, outcomes}];
        
      , {roll, diceOutcomes}];
      
      row
    ]
  ];
  
  transitionMat = Flatten[
    ParallelTable[
      buildStateRow[s, d],
      {s, 0, 39}, {d, 0, 2},
      Method -> "CoarsestGrained"
    ],
    1
  ];
  
  steadyState = First[NullSpace[Transpose[transitionMat] - IdentityMatrix[nStates]]];
  steadyState = steadyState / Total[steadyState];
  
  probs = Table[
    {s, Sum[steadyState[[toIndex[s, k]]], {k, 0, 2}]},
    {s, 0, 39}
  ];
  
  sorted = SortBy[probs, -Last[#] &];
  
  StringJoin[Map[IntegerString[#, 10, 2] &, Take[sorted[[All, 1]], 3]]]
];

solve[]