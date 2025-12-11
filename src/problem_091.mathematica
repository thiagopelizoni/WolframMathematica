(*
  Project Euler Problem 91: Right Triangles with Integer Coordinates
  URL: https://projecteuler.net/problem=091

  Problem Statement:
  We are looking for the number of right-angled triangles formed by the points O(0,0), P(x1, y1), and Q(x2, y2) with
  integer coordinates in the range 0 <= x, y <= 50. P and Q must be distinct from O and from each other. The right
  angle can be located at O, P, or Q.

  Mathematical Analysis:
  Let the grid size be N = 50. A triangle is defined by the set of vertices {O, P, Q}. Since a triangle can have at
  most one right angle, the sets of triangles with the right angle at O, P, and Q are disjoint.
  By symmetry, the number of triangles with the right angle at P is equal to the number of triangles with the right
  angle at Q (since P and Q iterate over the same domain).
  However, since we iterate over all possible coordinates for the "vertex with the right angle" (let's call it V),
  we simply need to sum the counts of valid configurations for each position of V.
  
  Total Triangles = Count(Angle at O) + Count(Angle at V != O).
  
  1. Right Angle at O(0,0):
     The legs must lie on the axes. One vertex is on the x-axis (x,0), the other on the y-axis (0,y).
     For x in 1..N and y in 1..N, there are N * N such triangles.
  
  2. Right Angle at V(x, y) where V != O:
     We iterate V over all valid points in the grid (excluding O).
     
     Case 2a: V is on the axes (e.g., (x, 0) or (0, y)).
     - If V=(x,0), the perpendicular is vertical. The other vertex must be (x, y') with y' in 1..N.
       (N choices for x) * (N choices for y') = N^2.
     - If V=(0,y), the perpendicular is horizontal. The other vertex must be (x', y) with x' in 1..N.
       (N choices for y) * (N choices for x') = N^2.
     Total for axes = 2 * N^2.

     Case 2b: V is strictly internal (1 <= x, y <= N).
     - We count points Q such that vector VQ is perpendicular to OV.
     - Let V = (x, y). Vector OV = (x, y). Perpendicular direction has slope -x/y.
     - Reduced step vector (dx, dy) = (x/g, y/g) where g = GCD(x, y).
     - Integer points Q lie at V + k * (-dy, dx) or V + k * (dy, -dx).
     - We calculate the range of integer k != 0 such that Q stays within [0, N]x[0, N].
     - Summing the valid k values gives the count for a specific V.
  
  Final Sum = (Count at O) + (Count at V on axes) + (Count at V internal).
  
  Computational Complexity:
  The algorithm iterates over N^2 internal points. For each, GCD and division are O(log N).
  Total complexity O(N^2 log N). With N=50, this is roughly 2500 operations, which is trivial.
  
  Parallelization:
  The summation for Case 2b is embarrassingly parallel. We distribute the loop over x (1 to N) across available cores.

  Wolfram Language Implementation:
  We define a `solve` function. We use `ParallelSum` for the internal points. We use `Floor` and `Min` to compute the
  bounds for k in O(1) for each point.
*)

solve[] := Module[{
  n = 50,
  countAngleAtO,
  countAngleAtAxes,
  countAngleAtInternal,
  totalTriangles,
  nCores
},
  nCores = $ProcessorCount;

  countAngleAtO = n * n;

  countAngleAtAxes = 2 * n * n;

  countAngleAtInternal = ParallelSum[
    Module[{g, dx, dy, kUpperLeft, kLowerRight},
      g = GCD[x, y];
      dx = x / g;
      dy = y / g;
      
      kUpperLeft = Min[Floor[x / dy], Floor[(n - y) / dx]];
      
      kLowerRight = Min[Floor[(n - x) / dy], Floor[y / dx]];
      
      kUpperLeft + kLowerRight
    ],
    {x, 1, n},
    {y, 1, n},
    Method -> "CoarsestGrained"
  ];

  totalTriangles = countAngleAtO + countAngleAtAxes + countAngleAtInternal;

  totalTriangles
];

solve[]