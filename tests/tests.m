AppendTo[$Path,$InitialDirectory <> "/../src/"];

<<affine.m;

Expect["Dimension equals to length",3,makeFiniteWeight[{1,2,3}][dimension]]

Expect["Scalar product for vectors from weight space of finite-dimensional Lie algebras",
       10,makeFiniteWeight[{1,2,3}].makeFiniteWeight[{3,2,1}]]

Expect["Plus for finite-dimensional weights",{2,4,6},(makeFiniteWeight[{1,2,3}]+makeFiniteWeight[{1,2,3}])[standardBase]]

(*
Expect["Plus  for vectors from different spaces are left unevaluated",True,
       MatchQ[makeFiniteWeight[{1,2,3}]+makeFiniteWeight[{3,2,1,2}],x_finiteWeight + y_finiteWeight]]
*)

Expect["Equal for finite weights compares standard base representations", 
       False,
       makeFiniteWeight[{1,2,3}]==makeFiniteWeight[{1,3,2}]]

Expect["Equal for finite weights compares standard base representations", 
       True,
       makeFiniteWeight[{1,2,3}]==makeFiniteWeight[{1,2,3}]]

Expect["Equal for finite weights compares standard base representations", 
       False,
       makeFiniteWeight[{1,2,3}]==makeFiniteWeight[{1,2,3,4}]]

Expect["Multiplication by scalar", True,makeFiniteWeight[{1,2,3}]*2==makeFiniteWeight[{2,4,6}]]

Expect["Multiplication by scalar", True,2*makeFiniteWeight[{1,2,3}]==makeFiniteWeight[{2,4,6}]]

Expect["Affine weight has the same real dimension as the finite-dimensional part, since we hold level and grade separetely",
       True,makeAffineWeight[makeFiniteWeight[{1,2,3,4,5}],1,2][dimension]==Length[{1,2,3,4,5}]]

Expect["Shortened constructor",
       True,makeAffineWeight[{1,2,3,4,5},1,2][dimension]==Length[{1,2,3,4,5}]]


Expect["Equal for affine weights compares finite parts, levels and grades", True,makeAffineWeight[makeFiniteWeight[{1,2,3}],1,2]==makeAffineWeight[makeFiniteWeight[{1,2,3}],1,2]]

Expect["Equal for affine weights compares finite parts, levels and grades", False,makeAffineWeight[makeFiniteWeight[{1,2,3}],2,1]==makeAffineWeight[makeFiniteWeight[{1,2,3}],1,2]]

Expect["Plus for affine weights",{2,4,6},(makeAffineWeight[makeFiniteWeight[{1,2,3}],1,2]+ makeAffineWeight[makeFiniteWeight[{1,2,3}],3,1])[finitePart][standardBase]]

Expect["Plus for affine weights",4,(makeAffineWeight[makeFiniteWeight[{1,2,3}],1,2]+ makeAffineWeight[makeFiniteWeight[{1,2,3}],3,1])[level]]

(*
   Expect["We compare dimensions of vectors before sum calculation, the expression is left unevaluated in case of dimension mismatch ",
   True,MatchQ[makeAffineWeight[makeFiniteWeight[{1,2}],1,2] + makeAffineWeight[ makeFiniteWeight[{3,2,1}],2,1], x_affineWeight + y_affineWeight]]
   *)

Expect["Scalar product for vectors from weight space of affine Lie algebras",20,
       makeAffineWeight[makeFiniteWeight[{1,2,3}],1,2]. 
       makeAffineWeight[makeFiniteWeight[{3,2,1}],3,4]]
    (*
       Expect["We compare dimensions of vectors before product calculation, the expression is left unevaluated in case of dimension mismatch ",
       True,MatchQ[makeAffineWeight[makeFiniteWeight[{1,2}],1,2]. makeAffineWeight[ makeFiniteWeight[{3,2,1}],2,1], x_affineWeight . y_affineWeight]]
       *)

Expect["Multiplication by scalar", True,makeAffineWeight[makeFiniteWeight[{1,2,3}],1,2]*2==makeAffineWeight[makeFiniteWeight[{2,4,6}],2,4]]

Module[{ht,tt},
       ht=makeHashtable[{"a",2,tt},{1,2,3}];
       Expect["Hashtable test",1,ht["a"]];
       Expect["Hashtable test",3,ht[tt]];
       Expect["Hashtable test",2,ht[2]]]

Expect["prependZeros",True,makeFiniteWeight[{0,0,1,1}]==prependZeros[2,makeFiniteWeight[{1,1}]]]

Expect["appendZeros",True,makeFiniteWeight[{1,1,0,0,0}]==appendZeros[3,makeFiniteWeight[{1,1}]]]

Module[{b2=makeSimpleRootSystem[B,2],a3=makeSimpleRootSystem[A,3]},
       Expect["Direct sum of finite-dimensional Lie algebras",True,
	      CirclePlus[b2,a3]==makeFiniteRootSystem[{{1,-1,0,0,0,0},
						       {0,1,0,0,0,0},
						       {0,0,1,-1,0,0},
						       {0,0,0,1,-1,0},
						       {0,0,0,0,1,-1}}]]]

Module[{b2=makeSimpleRootSystem[B,2],a3=makeSimpleRootSystem[A,3]},
       Expect["Direct sum of affine Lie algebras",True,
	      CirclePlus[makeAffineExtension[b2] , makeAffineExtension[a3]]==makeAffineExtension[makeFiniteRootSystem[{{1,-1,0,0,0,0},
														       {0,1,0,0,0,0},
														       {0,0,1,-1,0,0},
														       {0,0,0,1,-1,0},
														       {0,0,0,0,1,-1}}]]]]
Expect["B2:",True,makeSimpleRootSystem[B,2][simpleRoots][[1]]==makeFiniteWeight[{1,-1}]]

Expect["B2: rank",2,makeSimpleRootSystem[B,2][rank]]


Print["Hi!"]