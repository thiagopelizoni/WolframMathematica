(*
Project Euler Problem 62: Cubic Permutations
URL: https://projecteuler.net/problem=062

Problem Statement:
The problem asks for the smallest cube for which exactly five permutations of its digits are also
cubes. We are given the example of 41063625 (345^3), which is the smallest cube with exactly three
such permutations.

Mathematical Analysis:
Let S(x) be the signature of an integer x, defined as the sorted list of its digits. We are looking
for the minimum cube c = n^3 such that the set of cubes C_sig = {k^3 | S(k^3) = S(n^3)} has a
cardinality of exactly 5.
Permutations of digits preserve the total number of digits (assuming standard decimal representation
without leading zeros). Therefore, we can stratify the search by the number of digits, d, of the
cubes. For a fixed digit length d, the base n satisfies 10^(d-1) <= n^3 < 10^d, which implies n is
in the interval [Ceiling(10^((d-1)/3)), Floor((10^d - 1)^(1/3))].
The algorithm iterates through d = 1, 2, ... sequentially. For each d, we generate all cubes in the
interval, compute their signatures, and store them in an associative array (hash map) grouping cubes
by signature. If we find any group with size 5, the global minimum must be contained within the
results for this d, because any cube with d+1 digits is strictly larger than any cube with d digits.

Complexity & Feasibility:
The solution is known to be around 10^12 (12 digits), meaning bases n are around 10^4.
For d=12, the range of n is roughly [4642, 10000]. Generating, sorting digits, and hashing ~5000
integers is computationally trivial (O(N * d * log d)). The approach fits easily within milliseconds
on a modern CPU.

Parallelization Strategy:
We employ data parallelism in the generation phase. For a fixed d, the calculation of
{Signature(n^3), n^3} for all n in the range is independent. We use `ParallelTable` to distribute
this workload across all available cores. The reduction step (grouping by signature) is performed
centrally, as it relies on a hash map which is not efficiently parallelizable for small N, and the
cost is negligible compared to generation.

Implementation Details:
- Detect available cores using $ProcessorCount.
- Loop increasing digit length d.
- Compute integer bounds for n.
- Use `ParallelTable` to generate pairs of {sorted digits, cube}.
- Use `GroupBy` to aggregate cubes sharing the same signature.
- Use `Select` to find groups of size 5.
- Return the minimum cube found.
*)

nCores = $ProcessorCount;
solve[] := Module[{d = 1, lower, upper, cubes, grouped, candidates},
  While[True,
    lower = Ceiling[10^((d - 1)/3)];
    upper = Floor[N[(10^d - 1)^(1/3)]];

    cubes = ParallelTable[
      {Sort[IntegerDigits[n^3]], n^3},
      {n, lower, upper},
      Method -> "CoarsestGrained"
    ];

    grouped = GroupBy[cubes, First -> Last];

    candidates = Select[grouped, Length[#] == 5 &];

    If[Length[candidates] > 0,
      Return[Min[Flatten[Values[candidates]]]]
    ];

    d++;
  ]
];

solve[]