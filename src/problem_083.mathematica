(*
  Project Euler Problem 83: Path Sum: Four Ways
  URL: https://projecteuler.net/problem=083

  Problem Statement:
  Find the minimal path sum in an 80x80 matrix from the top-left cell (1, 1) to the bottom-right cell (80, 80).
  Unlike the previous problem, movement is allowed in all four directions: up, down, left, and right.

  Mathematical Analysis:
  This problem maps directly to finding the shortest path in a weighted directed graph G = (V, E).
  - Vertices V correspond to the matrix cells (i, j).
  - Edges E exist between adjacent cells (4-connectivity).
  - The "weight" of a path is the sum of the integers in the cells visited.
  Standard graph algorithms define path weight as the sum of edge weights. We can adapt this by setting the weight of
  the directed edge u -> v to be the value of the matrix at cell v.
  The total path sum will then be: Value(Start) + Sum(Edge Weights in Path).
  
  Since all matrix values are positive integers, Dijkstra's Algorithm is optimal and guaranteed to find the global minimum.
  The graph has N^2 vertices and approximately 4N^2 edges. For N=80, V=6400, E~25000.
  Dijkstra's complexity is O(E + V log V), which is computationally trivial for this size.

  Parallelization Strategy:
  While Dijkstra's" algorithm is inherently sequential (greedy wavefront expansion), the construction of the graph
  data structure (generating edges and weights from the matrix) is embarrassingly parallel.
  We decompose the matrix grid into rows. We assign a subset of rows to each available processor core.
  Each core generates the directed edges and corresponding weights for the cells in its assigned rows.
  The results are aggregated to construct a Wolfram Language `Graph` object, which is then solved using the optimized
  internal `GraphDistance` function.

  Wolfram Language Implementation:
  - Import the data from the official Project Euler resource.
  - Define a mapping from 2D matrix coordinates to 1D vertex indices.
  - Use `ParallelTable` to generate the list of weighted edges `{DirectedEdge[u, v], weight}`.
  - Construct the `Graph` object with `EdgeWeight`.
  - Compute `GraphDistance` from start to end and add the value of the starting cell (since edge weights only account
    for the cost of 'entering' the next node).
*)

solve[] := Module[{
  nCores, url, matrix, nRows, nCols,
  toIndex, edgesAndWeights, graph, pathCost
},
  nCores = $ProcessorCount;
  url = "https://projecteuler.net/project/resources/p083_matrix.txt";
  
  matrix = Import[url, "CSV"];
  {nRows, nCols} = Dimensions[matrix];

  toIndex = Function[{r, c}, (r - 1) * nCols + c];

  edgesAndWeights = Flatten[
    ParallelTable[
      Module[{idx, neighbors, localEdges},
        idx = toIndex[r, c];
        localEdges = {};
        
        If[r > 1, 
          AppendTo[localEdges, {DirectedEdge[idx, toIndex[r - 1, c]], matrix[[r - 1, c]]}]];
        If[r < nRows, 
          AppendTo[localEdges, {DirectedEdge[idx, toIndex[r + 1, c]], matrix[[r + 1, c]]}]];
        If[c > 1, 
          AppendTo[localEdges, {DirectedEdge[idx, toIndex[r, c - 1]], matrix[[r, c - 1]]}]];
        If[c < nCols, 
          AppendTo[localEdges, {DirectedEdge[idx, toIndex[r, c + 1]], matrix[[r, c + 1]]}]];
        
        localEdges
      ],
      {r, 1, nRows}, {c, 1, nCols},
      Method -> "CoarsestGrained"
    ],
    2
  ];

  graph = Graph[
    edgesAndWeights[[All, 1]], 
    EdgeWeight -> edgesAndWeights[[All, 2]]
  ];

  pathCost = GraphDistance[graph, 1, nRows * nCols];

  IntegerPart[pathCost + matrix[[1, 1]]]
]

solve[]