(* Project Euler Problem 121 — https://projecteuler.net/problem=121
   
   Disc Game Prize Fund
   
   A bag initially holds one red and one blue disc. Each turn a disc
   is drawn at random, its colour noted, then returned; an extra red
   disc is then added. After n turns the player wins if they drew
   more blue discs than red. On turn k (1-indexed) the bag contains
   k red and 1 blue disc, so P(blue on turn k) = 1/(k+1) and
   P(red on turn k) = k/(k+1). Given n = 15 turns, find the maximum
   whole-pound prize fund (including the stake) the banker may offer
   without expecting a loss, i.e. floor(1/P(win)).
   
   Mathematical analysis:
   The probability of any specific outcome is a product of n
   independent Bernoulli trials with varying success rates. The
   common denominator of all 2^n outcome probabilities is
   D = product_{k=1}^{n} (k+1) = (n+1)!. The numerator contributed
   by a subset S of "blue turns" (|S| = j) is product_{k not in S} k.
   The winning condition is j >= ceil((n+1)/2) = 8 for n = 15.
   
   The generating function product_{k=1}^{n} (x + k) encodes these
   sums: its coefficient of x^j equals e_{n-j}(1,...,n), which is
   the sum of prod_{k in T} k over all (n-j)-element subsets T of
   {1,...,n}. This coincides with the sum over size-j blue subsets S
   of the red-turns product. The winning numerator is therefore the
   sum of the coefficients of x^8 through x^15. The answer is
   floor(16! / numerator).
   
   The polynomial expansion costs O(n^2) integer arithmetic — all
   coefficients are moderate-sized integers — and is instantaneous
   for n = 15.
   
   Parallelization strategy:
   We split the 15 linear factors into nCores groups and compute
   partial polynomial products in parallel via ParallelMap. The
   partial results (coefficient lists) are then multiplied together
   sequentially using polynomial convolution. This distributes the
   bulk of the arithmetic across all available cores.
   
   Implementation plan:
   Detect cores, launch kernels. Partition the list of factors
   {(x+1),...,(x+15)} into balanced sublists, expand each partial
   product in parallel, convolve the results, extract the winning
   coefficients (x^8..x^15), sum them, and return floor(16!/sum).
*)

nCores = $ProcessorCount;
LaunchKernels[];

polyMul[a_List, b_List] := Module[{la = Length[a], lb = Length[b], c},
  c = ConstantArray[0, la + lb - 1];
  Do[
    Do[
      c[[i + j - 1]] += a[[i]] b[[j]],
      {j, 1, lb}
    ],
    {i, 1, la}
  ];
  c
]

partialPoly[factors_List] := Fold[polyMul, {1}, factors]

DistributeDefinitions[polyMul, partialPoly];

solve[] := Module[{n = 15, factors, groups, partials, poly, winNum},
  factors = Table[{k, 1}, {k, 1, n}];
  groups = Partition[factors, UpTo[Ceiling[n/nCores]]];
  partials = ParallelMap[partialPoly, groups];
  poly = Fold[polyMul, partials];
  winNum = Total[poly[[9 ;; 16]]];
  Floor[(n + 1)!/winNum]
]

solve[]
