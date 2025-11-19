(* Problem: https://projecteuler.net/problem=31 *)
(* Coin sums *)

(*
   Problem: In the United Kingdom the currency is made up of pound (£) and pence (p).
   There are eight coins in general circulation:
   1p, 2p, 5p, 10p, 20p, 50p, £1 (100p), and £2 (200p).
   
   It is possible to make £2 in the following way:
   1×£1 + 1×50p + 2×20p + 1×5p + 1×2p + 3×1p
   
   How many different ways can £2 be made using any number of coins?
   
   ---------------------------------------------------------------------------
   
   Mathematical approach: This is a classic dynamic programming problem known as
   the "coin change problem" or "making change problem". We want to count the
   number of ways to make a target amount using a given set of coin denominations.
   
   Let ways[n] = number of ways to make amount n using the available coins.
   
   Base case: ways[0] = 1 (one way to make 0: use no coins)
   
   Recurrence relation: For each coin denomination c, if we use that coin, we
   need to make the remaining amount (n - c). Thus:
     ways[n] = sum of ways[n - c] for all coins c where c ≤ n
   
   However, this naive approach counts the same combination multiple times
   (e.g., 1p+2p and 2p+1p are counted as different).
   
   Solution: Process coins in a specific order to avoid duplicates. For each coin
   denomination, update all amounts that can be formed by adding that coin to
   previously computed combinations.
   
   Algorithm (dynamic programming):
   1. Initialize array: ways[0] = 1, ways[1..200] = 0
   2. For each coin denomination (in any fixed order):
      For each amount i from coin value to target:
        ways[i] += ways[i - coin]
   3. Return ways[200]
   
   Why this works: By processing coins in order, we ensure that when we consider
   a coin, we only add it to combinations that use coins we've already processed.
   This guarantees each distinct combination is counted exactly once.
   
   Example for target = 5 with coins {1, 2, 5}:
   After processing 1p: ways = [1,1,1,1,1,1] (all combinations using only 1p)
   After processing 2p: ways = [1,1,2,2,3,3] (add combinations with 2p)
   After processing 5p: ways = [1,1,2,2,3,4] (add the single 5p coin)
   Result: 4 ways to make 5p
*)

totalAmount = 200;
coins = {1, 2, 5, 10, 20, 50, 100, 200};

countWays[total_, coinDenominations_] := Module[
  {ways, coin, i},
  
  ways = ConstantArray[0, total + 1];
  ways[[1]] = 1;
  
  Do[
    Do[
      ways[[i]] += ways[[i - coin]],
      {i, coin + 1, total + 1}
    ],
    {coin, coinDenominations}
  ];
  
  ways[[total + 1]]
]

countWays[totalAmount, coins]
