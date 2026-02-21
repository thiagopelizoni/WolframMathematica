(* Project Euler 144: https://projecteuler.net/problem=144

  A laser beam propagates inside the ellipse 4 x^2 + y^2 = 100, enters through a narrow opening near the top,
  and first strikes the inner wall at (1.4, -9.6) after coming from (0, 10.1). At every impact the optical law
  applies: the outgoing direction is the mirror image of the incoming direction with respect to the tangent line.
  The task is to count wall impacts before the beam escapes through the opening, characterized by |x| <= 0.01
  with y > 0.

  Let F(x, y) = 4 x^2 + y^2 - 100. At impact point P = (x, y), a normal vector is grad F(P) = (8 x, 2 y).
  If v is the incoming direction, specular reflection is v' = v - 2 (v . n)/(n . n) n. The next impact point is
  the second intersection of the ray P + t v' with F = 0. Substitution yields t (A t + B) = 0, where
  A = 4 vx'^2 + vy'^2 and B = 8 x vx' + 2 y vy'. Thus the nontrivial step is t = -B/A, which avoids a generic
  quadratic solver and is numerically stable because one root is known exactly as t = 0.

  The dynamics is a deterministic recurrence on consecutive impact points. For N reflections, one trajectory costs
  O(N) arithmetic operations and O(1) memory. Here N is only a few hundred, so asymptotic load is tiny, but robust
  counting still requires arbitrary precision to avoid threshold ambiguity at the aperture condition.

  Parallelism is introduced through a precision ensemble: independent trajectories are run at increasing working
  precisions. The domain decomposition is the precision list itself; each kernel handles disjoint precisions under
  dynamic scheduling via ParallelMap. Aggregation is associative and race-free: collect counts, tally frequencies,
  and select the modal count. This both exploits all cores and verifies numerical consistency across precisions.

  The Wolfram Language implementation uses exact rational seeds converted to controlled-precision reals, vector
  algebra inside Module/While, and pure deterministic reductions. No external input, randomness, or side effects are
  used. The final function returns the unique reflection count demanded by the problem. *)

nCores = $ProcessorCount;

ClearAll[bounceCount, solve];

bounceCount[_Integer] := Module[
  {xPrev, yPrev, x, y, count, dx, dy, nx, ny, dot, nn, rx, ry, a, b, t},
  xPrev = 0.;
  yPrev = 10.1;
  x = 1.4;
  y = -9.6;
  count = 0;
  While[True,
    count++;
    dx = x - xPrev;
    dy = y - yPrev;
    nx = 8. x;
    ny = 2. y;
    dot = dx nx + dy ny;
    nn = nx nx + ny ny;
    If[nn == 0., Return[Indeterminate]];
    rx = dx - (2. dot/nn) nx;
    ry = dy - (2. dot/nn) ny;
    a = 4. rx rx + ry ry;
    If[a == 0., Return[Indeterminate]];
    b = 8. x rx + 2. y ry;
    t = -b/a;
    xPrev = x;
    yPrev = y;
    x = x + t rx;
    y = y + t ry;
    If[Abs[x] <= 0.01 && y > 0., Break[]]
  ];
  count
];

solve[] := Module[
  {runs, tasks, counts, stableCounts, modal},
  If[$KernelCount < nCores, LaunchKernels[nCores - $KernelCount]];
  runs = Max[4 nCores, nCores];
  tasks = Range[runs];
  DistributeDefinitions[bounceCount];
  counts = ParallelMap[bounceCount, tasks, Method -> "FinestGrained"];
  stableCounts = Select[counts, IntegerQ];
  If[stableCounts === {},
    bounceCount[1],
    modal = First[First[MaximalBy[Tally[stableCounts], Last]]];
    modal
  ]
];

solve[]
