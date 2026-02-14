(*
Project Euler 109 — https://projecteuler.net/problem=109

The task is to count distinct checkout combinations in darts for totals strictly below 100 under standard
"doubles out" rules. A checkout is a sequence of at most three darts whose last dart is a double, including
double bull (50). Distinctness is by hit regions, not only by numerical scores. The first two darts are an
unordered multiset, while the final dart is distinguished by position; hence exchanging only the first two
darts does not create a new checkout, but changing the finishing double does.

Model each target region as a symbolic scoring type with integer value: singles S1..S20 and S25, doubles
D1..D20 and D25, trebles T1..T20. The finishing set is exactly the doubles. Since misses are excluded from
the combinational representation, missing darts are absorbed by allowing fewer than two preliminary darts.
Therefore each checkout is uniquely represented as (A, B, F), where F is one finishing double, and {A, B}
is an unordered multiset of size 0, 1, or 2 drawn from the 62 nonzero regions.

The counting reduces to additive combinatorics on these finite score multisets. Let c(t) be the number of
unordered preliminary choices with total t (including the empty choice contributing c(0)=1). Then the answer
is Sum_{f in Doubles} Sum_{t >= 0, f+t < 100} c(t). Correctness follows from bijection: each legal checkout
maps to exactly one finishing double and one unordered preliminary multiset, and conversely each such pair
defines a legal checkout under the game constraints.

Complexity is modest but structured. Building c(t) requires all unordered pairs with repetition from 62
regions, namely 62*63/2 = 1953 pair objects, plus singletons and the empty set; this is O(m^2) with m=62.
Aggregation over totals and finishing doubles is O(U*d), where U is the maximal relevant preliminary sum and
d=21 doubles. Both bounds are tiny, and exact integer arithmetic keeps the computation deterministic.

Parallelism is embarrassingly parallel in two places. First, pair sums are partitioned by first index i,
yielding disjoint chunks computed independently and joined by concatenation. Second, score-wise checkout
counts for totals 2..99 are independent and evaluated in parallel, then reduced by Total. The reductions are
associative and side-effect free, so no shared mutable state is required. In Wolfram Language we use
ParallelTable and ParallelMap over immutable lists, Tally for frequency aggregation, and Association lookups
for O(1)-style retrieval of c(t) values.
*)

ClearAll[solve];

solve[] := Module[
	{
		nCores,
		singles,
		doubles,
		trebles,
		allThrows,
		values,
		n,
		pairSums,
		starterSums,
		starterCount,
		scores,
		perScoreCounts
	},
	nCores = $ProcessorCount;
	LaunchKernels[nCores];
	singles = Join[Range[20], {25}];
	doubles = Join[2 Range[20], {50}];
	trebles = 3 Range[20];
	allThrows = Join[singles, doubles, trebles];
	values = allThrows;
	n = Length[values];
	DistributeDefinitions[values, n, doubles];
	pairSums = Flatten[
		ParallelTable[
			Table[
				values[[i]] + values[[j]],
				{j, i, n}
			],
			{i, 1, n}
		],
		1
	];
	starterSums = Join[{0}, values, pairSums];
	starterCount = Counts[starterSums];
	scores = Range[2, 99];
	perScoreCounts = ParallelMap[
		Function[s,
			Total[
				Lookup[
					starterCount,
					Select[s - doubles, # >= 0 &],
					0
				]
			]
		],
		scores,
		Method -> "CoarsestGrained"
	];
	Total[perScoreCounts]
]

solve[]
