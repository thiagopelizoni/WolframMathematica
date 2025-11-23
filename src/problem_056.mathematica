(* Problem: Project Euler #56 (Powerful Digit Sum)
   Context: Exploration of the maximal digital sum for the sequence a^b where a, b < 100.
   The problem demands arbitrary-precision arithmetic to handle terms up to 99^99 (approx. 197 decimal digits).
   While the search space is relatively compact (|S| ~ 10^4), computational density is non-trivial due to 
   large integer expansion and base-10 decomposition.

   Strategy: We perform a brute-force search over the Cartesian product of the domain. 
   To ensure scalability and adhere to the hardware-aware requirement, the evaluation of the 
   power-sum functional is distributed across all available logical processors using a 
   coarse-grained scheduling method to minimize IPC latency.
*)

If[Length[Kernels[]] < $ProcessorCount, LaunchKernels[$ProcessorCount]];

Max[
   ParallelTable[
      Total[IntegerDigits[a^b]],
      {a, 1, 99},
      {b, 1, 99},
      Method -> "CoarsestGrained"
   ]
]