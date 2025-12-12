(*
  Project Euler Problem 93: Arithmetic Expressions
  URL: https://projecteuler.net/problem=093

  Problem Statement:
  We are to consider sets of four distinct digits {a, b, c, d} chosen from 0 to 9. Using the arithmetic operations
  (+ , -, *, /) and parentheses, we form expressions using each digit exactly once. We seek the set {a, b, c, d} that
  produces the longest consecutive sequence of positive integers 1, 2, ..., n among its generated values. The answer
  must be the concatenation of the digits of this optimal set in increasing order.

  Mathematical Analysis:
  The problem space is small and amenable to exhaustive search.
  1. There are Binomial[10, 4] = 210 ways to choose 4 distinct digits.
  2. For each set of 4 digits, there are 4! = 24 permutations.
  3. There are 4^3 = 64 ways to choose operators for the 3 positions between digits.
  4. There are Catalan(3) = 5 distinct ways to group the operations with parentheses:
     ((A op B) op C) op D, (A op (B op C)) op D, (A op B) op (C op D),
     A op ((B op C) op D), A op (B op (C op D)).
  
  Total evaluations per set: 24 * 64 * 5 = 7680 expressions.
  Total operations for the entire search: 210 * 7680 = 1,612,800 expressions.
  This is trivial for modern CPUs (~10^6 ops). We can compute exact Rational values for all expressions to avoid
  floating-point precision issues, filtering for positive Integers at the end.

  Parallelization Strategy:
  The calculation for each set of 4 digits is independent. We can distribute the 210 sets across available processor
  cores using `ParallelMap`. Inside each parallel task, we generate all target values, determine the length of the
  consecutive sequence 1..n, and return the pair {sequence_length, digit_set}. Aggregation is performed by sorting
  the results to find the maximum length.

  Wolfram Language Implementation:
  - Use `Subsets` to generate the 210 digit combinations.
  - Use `Tuples` for operators and `Permutations` for digit ordering.
  - Define the 5 template structures explicitly as pure functions or expressions.
  - Use `Quiet` to suppress division-by-zero messages.
  - Use `Rational` arithmetic (native to Wolfram Language) to ensure exactness.
  - `solve[]` encapsulates the logic and returns the formatted string.
*)

solve[] := Module[{nCores, digitSets, operators, calcSequenceLength, results, bestSet},
  nCores = $ProcessorCount;
  
  (* Generate all unique sets of 4 distinct digits *)
  digitSets = Subsets[Range[0, 9], {4}];
  
  (* All combinations of 3 operators from {+, -, *, /} *)
  operators = Tuples[{Plus, Subtract, Times, Divide}, 3];
  
  (* Function to compute the longest consecutive sequence 1..n for a given set of digits *)
  (* This function is mapped in parallel over digitSets *)
  calcSequenceLength = Function[{digits},
    Module[{perms, values, validIntegers, maxN},
      perms = Permutations[digits];
      
      (* Generate all values using permutations, operators, and grouping templates *)
      values = Flatten @ Table[
        With[{a = p[[1]], b = p[[2]], c = p[[3]], d = p[[4]],
              op1 = ops[[1]], op2 = ops[[2]], op3 = ops[[3]]},
          Quiet @ {
            (* Template 1: ((a . b) . c) . d *)
            op3[op2[op1[a, b], c], d],
            (* Template 2: (a . (b . c)) . d *)
            op3[op1[a, op2[b, c]], d],
            (* Template 3: (a . b) . (c . d) *)
            op2[op1[a, b], op3[c, d]],
            (* Template 4: a . ((b . c) . d) *)
            op1[a, op3[op2[b, c], d]],
            (* Template 5: a . (b . (c . d)) *)
            op1[a, op2[b, op3[c, d]]]
          }
        ],
        {p, perms},
        {ops, operators}
      ];
      
      (* Filter for strictly positive integers *)
      validIntegers = Union @ Select[values, IntegerQ[#] && # > 0 &];
      
      (* Calculate length of consecutive sequence starting from 1 *)
      (* MapIndexed passes {value, {index}}; sequence holds if value == index *)
      maxN = LengthWhile[
        MapIndexed[{#1, #2[[1]]} &, Sort[validIntegers]], 
        #[[1]] == #[[2]] &
      ];
      
      {maxN, digits}
    ]
  ];

  (* Execute in parallel *)
  results = ParallelMap[calcSequenceLength, digitSets, Method -> "CoarsestGrained"];
  
  (* Find the set with the maximum sequence length *)
  (* Sort by length descending *)
  bestSet = First @ SortBy[results, {-First[#] &}];
  
  (* Format output as string "abcd" *)
  StringJoin[ToString /@ Sort[bestSet[[2]]]]
];

solve[]