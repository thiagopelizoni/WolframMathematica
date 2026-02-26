(* Project Euler 157: https://projecteuler.net/problem=157

For each n from 1 through 9, we must count positive integer triples (a,b,p) with a<=b and
1/a + 1/b = p/10^n, then sum those counts over all n. For fixed n and pair (a,b), p is
uniquely determined, so the problem is equivalent to counting pairs (a,b) such that
10^n (a+b)/(ab) is an integer.

Write a = g x and b = g y with g = gcd(a,b), gcd(x,y)=1, and x<=y. The divisibility
condition becomes g x y | 10^n (x+y). Since gcd(xy,x+y)=1 under gcd(x,y)=1, we must have
xy | 10^n, and then g can be any divisor of 10^n (x+y)/(xy). Hence each admissible coprime
pair (x,y) contributes exactly tau(10^n (x+y)/(xy)), where tau is the divisor-count
function.

Because 10^n = 2^n 5^n, both x and y are divisors of 10^n; admissibility is fully captured
by x<=y, gcd(x,y)=1, and xy|10^n. This replaces an unbounded search in (a,b) by a finite
enumeration over divisors of 10^n. Let D_n = (n+1)^2 be the divisor count of 10^n. The
candidate space is O(D_n^2), and each contribution is one divisor-count evaluation on an
integer of size O(10^n), giving O(sum_{n=1}^N D_n^2 log M_n) arithmetic work in practice.
For N=9 this is tiny and decisively feasible.

The reduced contributions are independent across admissible (n,x,y) tuples, so the heavy
summation is embarrassingly parallel. The implementation constructs all tasks, partitions
that list into near-equal chunks based on $ProcessorCount, evaluates chunk sums on separate
kernels with ParallelMap, and aggregates partial sums via Total. Associativity of addition
ensures deterministic, race-free reduction.

Implementation uses exact integer arithmetic only. Divisors are generated as products of
powers of 2 and 5; constraints are tested with CoprimeQ and Divisible; each contribution is
computed by DivisorSigma[0, ...] for tau. No external files, input, randomness, or side
output are used; solve[] returns the exact Project Euler value. *)

nCores = $ProcessorCount;

ClearAll[
  validTasksForN,
  taskContribution,
  solve
];

validTasksForN[n_Integer?Positive] := Module[
  {m = 10^n, divisors, len},
  divisors = Flatten@Table[2^i*5^j, {i, 0, n}, {j, 0, n}];
  len = Length[divisors];
  Flatten[
    Table[
      If[
        CoprimeQ[divisors[[i]], divisors[[j]]] && Divisible[m, divisors[[i]]*divisors[[j]]],
        {{m, divisors[[i]], divisors[[j]]}},
        {}
      ],
      {i, 1, len},
      {j, i, len}
    ],
    2
  ]
];

taskContribution[{m_Integer, x_Integer, y_Integer}] := DivisorSigma[
  0,
  Quotient[m, x*y]*(x + y)
];

solve[] := Module[
  {tasks, chunkSize},
  If[$KernelCount < nCores, LaunchKernels[nCores - $KernelCount]];
  tasks = Flatten[Table[validTasksForN[n], {n, 1, 9}], 1];
  chunkSize = Max[1, Ceiling[Length[tasks]/Max[1, nCores]]];
  DistributeDefinitions[taskContribution];
  Total[
    ParallelMap[
      Total[taskContribution /@ #] &,
      Partition[tasks, UpTo[chunkSize]],
      Method -> "FinestGrained"
    ]
  ]
];

solve[]
