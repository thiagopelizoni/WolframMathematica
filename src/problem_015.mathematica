(* Problem: https://projecteuler.net/problem=15 *)
(* Number of lattice paths through a grid. *)

(*
   Problem statement (Project Euler 15, paraphrased):
   
   Starting in the top left corner of a 2×2 grid, and only being able to move
   to the right and down, there are exactly 6 routes to the bottom right corner.
   
   How many such routes are there through a 20×20 grid?
   
   The known correct answer for a 20×20 grid is:
     137846528820

   ---------------------------------------------------------------------------
   Mathematical background: Combinatorics and Lattice Paths
   ---------------------------------------------------------------------------

   1) Problem setup:
      We have an n×n grid. We start at the top-left corner (0, 0) and want to
      reach the bottom-right corner (n, n).
      
      Movement rules:
      - We can only move RIGHT (R) or DOWN (D)
      - Each move increases exactly one coordinate by 1
      
      To reach from (0, 0) to (n, n), we need:
      - Exactly n moves to the RIGHT
      - Exactly n moves DOWN
      - Total: 2n moves

   2) Combinatorial interpretation:
      Any valid path can be encoded as a sequence of 2n moves, where exactly
      n of them are R (right) and n of them are D (down).
      
      Example for 2×2 grid (from (0,0) to (2,2)):
        RRDD - right, right, down, down
        RDRD - right, down, right, down
        RDDR - right, down, down, right
        DRRD - down, right, right, down
        DRDR - down, right, down, right
        DDRR - down, down, right, right
      Total: 6 paths
      
      The question becomes: "In how many ways can we choose n positions out of
      2n total positions for the R moves?" (The remaining positions will
      automatically be D moves.)

   3) Binomial coefficient:
      The number of ways to choose k items from n items (order doesn't matter)
      is given by the binomial coefficient:
      
        C(n, k) = n! / (k! × (n-k)!)
      
      Also written as (n choose k) or ⎛n⎞
                                      ⎝k⎠
      
      For our problem:
      - We need to choose n positions (for R moves) from 2n total positions
      - Number of ways = C(2n, n) = (2n)! / (n! × n!)

   4) Formula for n×n grid:
      Number of lattice paths from (0,0) to (n,n) = C(2n, n) = (2n)! / (n!)²

   5) Example verification:
      For n = 2:
        C(4, 2) = 4! / (2! × 2!)
                = 24 / (2 × 2)
                = 24 / 4
                = 6
      
      This matches our enumeration above! ✓

   6) For n = 20:
      C(40, 20) = 40! / (20! × 20!)
                = 137846528820

   7) Wolfram Language implementation:
      Wolfram Language has a built-in Binomial function that efficiently
      computes binomial coefficients without explicitly calculating large
      factorials (which would cause numerical overflow).
      
      Binomial[n, k] computes C(n, k) using optimized algorithms.

   ---------------------------------------------------------------------------
   Wolfram Language implementation
   ---------------------------------------------------------------------------
*)

latticePaths[n_Integer?NonNegative] := Module[
  {},
  
  (* The number of lattice paths in an n×n grid is the binomial coefficient
     C(2n, n), which counts the number of ways to arrange n right moves and
     n down moves in a sequence of 2n total moves.
     
     Mathematically: C(2n, n) = (2n)! / (n! × n!) = (2n)! / (n!)²
     
     Wolfram's Binomial function efficiently computes this without overflow. *)
  Binomial[2*n, n]
]

(* Calculate the number of lattice paths for a 20×20 grid.
   Expected answer: 137846528820 *)
latticePaths[20]
