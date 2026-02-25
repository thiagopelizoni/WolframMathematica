(* Project Euler 155: https://projecteuler.net/problem=155

  We have identical capacitors of value C. Using at most 18 units, and allowing only binary compositions by parallel or series connections,
  count how many distinct equivalent capacitances can be produced.

  Normalizing by C, every realizable value is a positive rational. Let E_n be the set of normalized capacitances obtainable with exactly n
  capacitors. With two subnetworks of sizes i and n-i carrying values a and b, the only binary composition laws are
  a+b (parallel) and ab/(a+b) (series). Hence
  E_n = Union_{i=1..floor(n/2)} {a+b, ab/(a+b) : a in E_i, b in E_{n-i}}.
  The index only runs to floor(n/2) because exchanging i and n-i does not create new values.

  The target quantity is D(18)=|Union_{k=1..18} E_k|. Exact arithmetic is mandatory: floating approximations would merge distinct rationals.
  Wolfram rationals are canonical reduced fractions, so equality testing and deduplication via Union are mathematically exact.

  Complexity is governed by pair products |E_i| |E_{n-i}|. For n=18 the total pair count over all split indices is about 8.3 million, creating
  roughly 16.6 million candidate rationals before deduplication. This is far below brute-force enumeration of all circuit trees and is
  computationally feasible with vectorized rational operations and staged deduplication.

  Parallelization is applied at the dominant independent decomposition: for fixed n, each split i versus n-i is a separate subproblem.
  Kernels evaluate split-combination maps independently and return candidate lists; aggregation is a deterministic Union reduction.
  Work partition is naturally balanced because split costs are of similar magnitude near the middle and still substantial near the edges.

  The implementation uses exact rationals, list-based dynamic programming for E_n, dynamic kernel launch up to $ProcessorCount - 1 with a
  bounded attempt, and deterministic serial fallback if subkernels are unavailable. The heavy combine step orients each split so the outer
  map iterates the smaller set, while the inner formulas are vectorized over the larger set to reduce interpreter overhead. *)

nCores = $ProcessorCount;

ClearAll[
  combineSplit,
  solve
];

combineSplit[left_List, right_List] := Module[
  {a, b},
  If[
    Length[left] <= Length[right],
    a = left;
    b = right,
    a = right;
    b = left
  ];
  Join @@ Map[
    Join[b + #, # b/(# + b)] &,
    a
  ]
];

solve[] := Module[
  {n = 18, exact, allValues, k, splitData, parts, kernelsTarget, workers},
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
  exact = ConstantArray[{}, n];
  exact[[1]] = {1};
  allValues = exact[[1]];
  If[workers > 1, DistributeDefinitions[combineSplit]];
  For[k = 2, k <= n, k++,
    splitData = Table[
      {exact[[i]], exact[[k - i]]},
      {i, 1, Floor[k/2]}
    ];
    parts = If[
      workers > 1,
      ParallelMap[
        combineSplit[#[[1]], #[[2]]] &,
        splitData,
        Method -> "FinestGrained"
      ],
      Map[
        combineSplit[#[[1]], #[[2]]] &,
        splitData
      ]
    ];
    exact[[k]] = Union @@ parts;
    allValues = Union[allValues, exact[[k]]];
  ];
  Length[allValues]
];

solve[]
