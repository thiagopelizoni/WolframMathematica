(*
  Project Euler Problem 92: Square Digit Chains
  URL: https://projecteuler.net/problem=092

  Problem Statement:
  A number chain is created by continuously adding the square of the digits in a number to form a new number until it
  has been seen before. It is stated that every starting number below 10,000,000 will eventually arrive at 1 or 89.
  We need to calculate how many starting numbers below 10,000,000 will arrive at 89.

  Mathematical Analysis:
  Let S(n) be the sum of the squares of the digits of n. For any n < 10^7, the maximum possible value for S(n) is
  S(9,999,999) = 9^2 * 7 = 567. Thus, any number in the target range reduces to a number <= 567 in one step. The chain
  destiny (1 or 89) for all integers up to 10^7 is determined by the destiny of integers in the range [1, 567].

  Instead of iterating through all 10^7 integers (O(N)), we use a combinatorial approach. The value of S(n) depends
  only on the multiset of digits of n, not their order. We can generate all distinct combinations of 7 digits (padding
  smaller numbers with leading zeros) and calculate S(n) for each combination. If S(n) leads to 89, we add the number
  of distinct permutations of those digits (calculated via the Multinomial coefficient) to the total count.
  
  The number of combinations with replacement of 7 digits chosen from {0..9} is Binomial[10+7-1, 7] = 11,440. This
  transforms the problem from 10^7 operations to ~1.1 * 10^4 operations, which is computationally trivial.

  Parallelization Strategy:
  The combinatorial check is embarrassingly parallel. We verify the available cores and use `ParallelSum` to iterate
  over the 11,440 digit combinations. The precomputed destiny table for numbers 1 to 567 is small enough to be
  distributed automatically to worker kernels.

  Wolfram Language Implementation:
  - Detect core count.
  - Precompute a lookup table for the destiny of numbers 1..567 using a simple iterative loop.
  - Use `FrobeniusSolve` to generate all non-negative integer solutions to c0 + c1 + ... + c9 = 7, representing
    digit counts.
  - Use `ParallelSum` to iterate over these solutions, calculating the sum of squared digits, checking the table,
    and adding the `Multinomial` count if the destiny is 89.
*)

solve[] := Module[{
  nCores, maxSqSum, squares, destinyTable, digitCombinations
},
  nCores = $ProcessorCount;
  
  (* Maximum sum of squares for a number < 10^7 is 9*9*7 = 567 *)
  maxSqSum = 567;
  squares = Range[0, 9]^2;
  
  (* Precompute destinations (1 or 89) for the range [1, 567] *)
  destinyTable = ConstantArray[0, maxSqSum];
  destinyTable[[1]] = 1;
  destinyTable[[89]] = 89;
  
  (* Fill the table using iterative resolution *)
  Do[
    If[destinyTable[[i]] == 0,
      Module[{path = {i}, curr = i, result},
        (* Follow chain until we hit 1, 89, or a known value *)
        While[
          curr != 1 && curr != 89 && 
          (curr > maxSqSum || destinyTable[[curr]] == 0),
          curr = Total[IntegerDigits[curr]^2];
          AppendTo[path, curr];
        ];
        
        (* Determine the final destination *)
        result = If[curr <= maxSqSum && destinyTable[[curr]] != 0, 
                   destinyTable[[curr]], 
                   curr (* Should be 1 or 89 *)
                 ];
        
        (* Memoize results for all steps in the path that fit in the table *)
        Scan[
          Function[n, If[n <= maxSqSum, destinyTable[[n]] = result]], 
          path
        ];
      ]
    ],
    {i, 1, maxSqSum}
  ];

  (* Generate all unique combinations of 7 digits.
     This is equivalent to finding integer partitions of 7 into 10 bins (counts of digits 0..9). *)
  digitCombinations = FrobeniusSolve[ConstantArray[1, 10], 7];

  (* Parallel summation over all combinations *)
  ParallelSum[
    Module[{counts = config, sumSq, result},
      (* Calculate sum of squares for this digit configuration.
         The configuration is a list of counts {c0, c1, ..., c9}. *)
      sumSq = counts . squares;
      
      (* Check destiny. Note: sumSq=0 (number 0) is excluded by problem constraints *)
      If[sumSq > 0 && destinyTable[[sumSq]] == 89,
        (* Add number of permutations for this set of digits *)
        Multinomial @@ counts,
        0
      ]
    ],
    {config, digitCombinations},
    Method -> "CoarsestGrained"
  ]
];

solve[]