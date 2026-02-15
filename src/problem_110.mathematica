(*
Project Euler 110 — https://projecteuler.net/problem=110

We must find the smallest positive integer n such that the equation 1/x + 1/y = 1/n has more than four
million distinct solutions in positive integers when symmetric pairs are identified.

Set x = n + a and y = n + b. Then (n + a) (n + b) = n (2 n + a + b), hence ab = n^2, equivalently
(x - n) (y - n) = n^2. Therefore each divisor pair d1 d2 = n^2 gives one ordered solution, and the number
of unordered solutions equals (tau(n^2) + 1)/2. The requirement is tau(n^2) > 7,999,999.

If n = Product p_i^e_i, then tau(n^2) = Product (2 e_i + 1). Minimizing n under a lower bound on this
multiplicative functional is a constrained integer optimization in prime exponents. By standard monotonicity
and rearrangement arguments, any minimizer has nonincreasing exponents on increasing primes:
e_1 >= e_2 >= ... >= 0. This reduces the search to finite exponent vectors in a branch-and-bound tree.

We use an explicit constructive bound from the feasible exponent profile (3,3,2,2,1,1,1,1,1,1,1,1), whose
odd-factor product is 8,037,225 and thus exceeds the target. This yields a finite incumbent n and aggressive
pruning. At each node we cap the next exponent by both monotonicity and the integer logarithmic budget from
the incumbent. A safe multiplicative upper estimate of reachable tau(n^2) from remaining primes provides
additional pruning; if that estimate is already below threshold, the subtree is discarded.

The dominant workload is the depth-first exploration of branches indexed by the first exponent e_1. These
branches are independent, so we distribute them across all kernels and reduce local minima with Min, an
associative deterministic aggregation. Integer arithmetic is exact throughout; no floating-point operations,
randomization, external input, or side effects are needed.

The resulting complexity is far below naive scanning. Practical cost is proportional to the number of viable
nonincreasing exponent prefixes that survive pruning, which is tiny at this threshold with the tight bound.
Hence the computation is fully feasible within Project Euler scale.
*)

ClearAll[solve];

solve[] := Module[
	{
		nCores,
		target = 4000000,
		threshold,
		primes,
		upperExponents,
		upperBound,
		firstExpMax,
		firstExponents,
		branchMinima
	},
	nCores = $ProcessorCount;
	LaunchKernels[nCores];
	threshold = 2*target - 1;
	primes = Prime[Range[20]];
	upperExponents = Join[{3, 3, 2, 2}, ConstantArray[1, 8]];
	upperBound = Times @@ (Prime[Range[Length[upperExponents]]]^upperExponents);
	firstExpMax = IntegerLength[upperBound - 1, 2] - 1;
	firstExponents = Range[1, firstExpMax];
	DistributeDefinitions[threshold, primes, upperBound];
	branchMinima = ParallelMap[
		Function[firstExp,
			Module[{best = upperBound, primeCount = Length[primes], intLog, recur},
				intLog[base_, m_] := If[m < 1, -1, IntegerLength[m, base] - 1];
				recur[idx_, maxExp_, currentN_, currentTau_] := Module[
					{
						limitRatio,
						p,
						eMax,
						e,
						n2,
						tau2,
						tailPrimes,
						tauCap
					},
					If[currentN >= best, Return[]];
					If[currentTau > threshold,
						best = currentN;
						Return[];
					];
					If[idx > primeCount, Return[]];
					limitRatio = Quotient[best - 1, currentN];
					If[limitRatio < 1, Return[]];
					tailPrimes = primes[[idx ;; primeCount]];
					tauCap = currentTau*Times @@ (2*Map[intLog[#, limitRatio] &, tailPrimes] + 1);
					If[tauCap <= threshold, Return[]];
					p = primes[[idx]];
					eMax = Min[maxExp, intLog[p, limitRatio]];
					If[eMax < 1, Return[]];
					For[e = eMax, e >= 1, e--,
						n2 = currentN*p^e;
						tau2 = currentTau*(2*e + 1);
						If[tau2 > threshold,
							If[n2 < best, best = n2],
							recur[idx + 1, e, n2, tau2]
						];
					];
				];
				recur[2, firstExp, 2^firstExp, 2*firstExp + 1];
				best
			]
		],
		firstExponents,
		Method -> "CoarsestGrained"
	];
	Min[branchMinima]
]

solve[]
