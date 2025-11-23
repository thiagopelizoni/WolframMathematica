(*
  Problem: Project Euler 59 - XOR Decryption.

  Mathematical Analysis:
  The problem involves the cryptanalysis of a message obfuscated by a periodic polyalphabetic 
  substitution cipher (specifically, a Vigenère variant utilizing the bitwise XOR involution). 
  The key space $\mathcal{K}$ is defined as the Cartesian product of three lowercase ASCII 
  integer sets, creating a search space of cardinality $|\mathcal{K}| = 26^3 = 17,576$. 
  Decryption is performed via the transformation $m_i = c_i \oplus k_{i \pmod 3}$.

  Computational Strategy:
  1. Data Ingestion: The ciphertext vector is imported and flattened into a list of integers.
  2. Parallel Search: Given the independence of trial decryptions, we distribute the candidate 
     keys across the available kernel pool ($ProcessorCount). A linguistic heuristic 
     (presence of the trigram " the ") serves as the acceptance predicate.
  3. Vectorization: The XOR operation is vectorized using `PadRight` with cyclic padding 
     to extend the key to the message length efficiently, avoiding procedural iteration.
  4. Reduction: The scalar ASCII sum of the recovered plaintext is computed upon identification 
     of the valid isomorphism.
*)

Module[{
    cipherUrl = "https://projecteuler.net/project/resources/p059_cipher.txt",
    cipherVector,
    keySpace,
    validKey,
    decryptedInts
  },
  
  If[Length[Kernels[]] < $ProcessorCount, LaunchKernels[$ProcessorCount]];

  cipherVector = Flatten[Import[cipherUrl, "CSV"]];
  
  keySpace = Tuples[Range[97, 122], 3];

  validKey = First @ ParallelSelect[
      keySpace,
      StringContainsQ[
          FromCharacterCode[
              BitXor[cipherVector, PadRight[#, Length[cipherVector], #]]
          ], 
          " the "
      ] &
  ];

  decryptedInts = BitXor[cipherVector, PadRight[validKey, Length[cipherVector], validKey]];

  Total[decryptedInts]
]