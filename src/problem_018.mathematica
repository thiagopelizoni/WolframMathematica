(* Problem: https://projecteuler.net/problem=18 *)
(* Maximum path sum through a triangle. *)

(*
   Problem statement (Project Euler 18, paraphrased):
   
   By starting at the top of the triangle below and moving to adjacent numbers
   on the row below, find the maximum total from top to bottom.
   
   Example small triangle:
        3
       7 4
      2 4 6
     8 5 9 3
   
   In this case, the maximum path is 3 + 7 + 4 + 9 = 23.
   
   For the given 15-row triangle, find the maximum path sum.

   ---------------------------------------------------------------------------
   Mathematical background: Dynamic Programming
   ---------------------------------------------------------------------------

   1) Problem structure:
      We have a triangle of numbers arranged in rows:
      - Row 0 (top): 1 element
      - Row 1: 2 elements
      - Row 2: 3 elements
      - Row k: k+1 elements
      
      Movement rules: From position (row, col), we can move to either:
      - (row+1, col) - directly below
      - (row+1, col+1) - diagonally below-right
      
      Goal: Find the path from top to bottom that maximizes the sum.

   2) Naive approach (exponential complexity):
      We could explore all possible paths from top to bottom.
      At each position we make a binary choice (left or right child).
      For a triangle with n rows, this gives approximately 2^n paths.
      For n=15, that's 32,768 paths - feasible but inefficient.
      For larger triangles (like Problem 67 with 100 rows), this becomes
      computationally infeasible (2^100 ≈ 10^30 paths).

   3) Dynamic programming approach (polynomial complexity):
      Key insight: The problem has optimal substructure.
      
      Define: M[r][c] = maximum sum achievable from position (r,c) to bottom
      
      Then: M[r][c] = triangle[r][c] + max(M[r+1][c], M[r+1][c+1])
      
      Base case: For the bottom row, M[n-1][c] = triangle[n-1][c]
      
      This recurrence relation allows us to build the solution bottom-up.

   4) Bottom-up algorithm:
      Instead of computing recursively with memoization, we modify the
      triangle in-place, working from bottom to top:
      
      - Start at the second-to-last row
      - For each element at position (r,c), replace it with:
          triangle[r][c] + max(triangle[r+1][c], triangle[r+1][c+1])
      - Move up row by row
      - When we reach the top, triangle[0][0] contains the answer
      
      This approach:
      - Runs in O(n²) time (we visit each element once)
      - Uses O(1) extra space (modifies triangle in-place)
      - Automatically finds the optimal path without explicitly tracking it

   5) Why this works:
      After processing row r, each element triangle[r][c] contains the
      maximum sum achievable starting from that position.
      
      By the time we reach the top, triangle[0][0] contains the maximum
      sum achievable from the very top - which is our answer.

   6) Example walkthrough (small triangle):
        3
       7 4
      2 4 6
     8 5 9 3
     
     Bottom row: {8, 5, 9, 3} (already optimal - base case)
     
     Row 2 processing:
       2 + max(8,5) = 10
       4 + max(5,9) = 13
       6 + max(9,3) = 15
     Result: {10, 13, 15}
     
     Row 1 processing:
       7 + max(10,13) = 20
       4 + max(13,15) = 19
     Result: {20, 19}
     
     Row 0 processing:
       3 + max(20,19) = 23
     Result: {23}
     
     Answer: 23 ✓

   ---------------------------------------------------------------------------
   Wolfram Language implementation
   ---------------------------------------------------------------------------
*)

(* Triangle data from Project Euler Problem 18 *)
triangle = {
  {75},
  {95, 64},
  {17, 47, 82},
  {18, 35, 87, 10},
  {20, 4, 82, 47, 65},
  {19, 1, 23, 75, 3, 34},
  {88, 2, 77, 73, 7, 63, 67},
  {99, 65, 4, 28, 6, 16, 70, 92},
  {41, 41, 26, 56, 83, 40, 80, 70, 33},
  {41, 48, 72, 33, 47, 32, 37, 16, 94, 29},
  {53, 71, 44, 65, 25, 43, 91, 52, 97, 51, 14},
  {70, 11, 33, 28, 77, 73, 17, 78, 39, 68, 17, 57},
  {91, 71, 52, 38, 17, 14, 91, 43, 58, 50, 27, 29, 48},
  {63, 66, 4, 68, 89, 53, 67, 30, 73, 16, 69, 87, 40, 31},
  {4, 62, 98, 27, 23, 9, 70, 98, 73, 93, 38, 53, 60, 4, 23}
};

maximumPathSum[tri_List] := Module[
  {
    workingTriangle, numRows, row, col
  },
  
  (* Create a working copy to avoid modifying the original triangle *)
  workingTriangle = tri;
  numRows = Length[workingTriangle];
  
  (* Dynamic programming: work from second-to-last row upward to top *)
  Do[
    (* For each element in the current row *)
    Do[
      (* Add the maximum of the two adjacent elements from the row below *)
      workingTriangle[[row, col]] += 
        Max[workingTriangle[[row + 1, col]], 
            workingTriangle[[row + 1, col + 1]]],
      {col, 1, Length[workingTriangle[[row]]]}
    ],
    {row, numRows - 1, 1, -1}
  ];
  
  (* After processing, the top element contains the maximum path sum *)
  workingTriangle[[1, 1]]
]

(* More functional approach using MapIndexed and FoldRight *)
maximumPathSumFunctional[tri_List] := Module[
  {
    processRow
  },
  
  (* Function to merge two adjacent rows *)
  processRow[upperRow_, lowerRow_] :=
    MapIndexed[
      #1 + Max[lowerRow[[#2[[1]]]], lowerRow[[#2[[1]] + 1]]]&,
      upperRow
    ];
  
  (* Fold from bottom to top, merging rows pairwise *)
  First[FoldRight[processRow, tri]]
]

(* Calculate the maximum path sum for the given triangle *)
maximumPathSum[triangle]
