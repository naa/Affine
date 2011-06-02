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

Expect["checkGrade finite dimensional Lie algebra", True, Affine`Private`checkGrade[makeSimpleRootSystem[B,2]][makeFiniteWeight[{2,1}]]]

Expect["checkGrade affine Lie algebra", False, Affine`Private`checkGrade[makeAffineExtension[makeSimpleRootSystem[A,2]]][makeAffineWeight[{2,1,1},1,10]]]

Module[{b2a=makeAffineExtension[makeSimpleRootSystem[B,2]]},
       b2a[gradeLimit]=15;
       Expect["checkGrade affine Lie algebra", True, Affine`Private`checkGrade[b2a][makeAffineWeight[{2,1},1,10]]];
       b2a[gradeLimit]=.;]

Expect["weight predicate on finite weights", True, weightQ[makeFiniteWeight[{1,2,3}]]]

Expect["weight predicate on finite weights", False, weightQ[makeFiniteWeight[1,2,3]]]

Expect["weight predicate on affine weights", True, weightQ[makeAffineWeight[{1,2,3},1,1]]]

Expect["weight predicate on affine weights", False, weightQ[makeAffineWeight[1,2,3]]]

Expect["Reflection for finite weights", True,reflection[makeFiniteWeight[{1,0}]][makeFiniteWeight[{1,1}]]==makeFiniteWeight[{-1,1}]]

Expect["Co root of [1,0]", True, coroot[makeFiniteWeight[{1,0}]]==makeFiniteWeight[{2,0}]]

Expect["Co root of affine [1,0]", True, coroot[makeAffineWeight[{1,0},1,0]]==makeAffineWeight[{2,0},2,0]]

Expect["Cartan matrix of B2",{{2, -1}, {-2, 2}},cartanMatrix[makeSimpleRootSystem[B,2]]]

Expect["Predicate for finite and affine root systems",True,rootSystemQ[makeSimpleRootSystem[B,2]]]

Expect["Weyl reflection s1 s2 s1 in algebra B2",True,
       weylGroupElement[makeSimpleRootSystem[B,2]][1,2,1][makeFiniteWeight[{1,0}]]==
       makeFiniteWeight[{-1,0}]]

Expect["Fundamental weights for B2", True, {makeFiniteWeight[{1,0}],makeFiniteWeight[{1/2,1/2}]}==fundamentalWeights[makeSimpleRootSystem[B,2]]]

Expect["Fundamental weights for A1", True, {makeFiniteWeight[{1/2}]}==fundamentalWeights[makeSimpleRootSystem[A,1]]]

Expect["Fundamental weights for A2", True, {makeFiniteWeight[{2/3,-1/3,-1/3}],makeFiniteWeight[{1/3,1/3,-2/3}]}==fundamentalWeights[makeSimpleRootSystem[A,2]]]


Expect["Weyl vector for B2",True,makeFiniteWeight[{3/2,1/2}]==rho[makeSimpleRootSystem[B,2]]]

Expect["Weyl vector for B2",True,makeFiniteWeight[{3/2,1/2}]==rho[positiveRoots[makeSimpleRootSystem[B,2]]]]

Expect["To fundamental chamber",True,makeFiniteWeight[{1,1/2}]==toFundamentalChamber[makeSimpleRootSystem[B,2]][makeFiniteWeight[{-1,1/2}]]]

Expect["We can use this and other functions for mapping",True,
       Map[toFundamentalChamber[makeSimpleRootSystem[B,2]],{makeFiniteWeight[{-1,-1}],makeFiniteWeight[{-2,-1}]}]==
       {makeFiniteWeight[{1, 1}], makeFiniteWeight[{2, 1}]}]

Expect["Main chamber predicate",True,mainChamberQ[makeSimpleRootSystem[B,2]][toFundamentalChamber[makeSimpleRootSystem[B,2]][makeFiniteWeight[{-1,1/2}]]]]

Module[{b2=makeSimpleRootSystem[B,2]},
       Expect["Partial orbit test", True, partialOrbit[b2][{rho[b2]}]==
	      {{makeFiniteWeight[{3/2,1/2}]},{makeFiniteWeight[{1/2,3/2}],makeFiniteWeight[{3/2,-1/2}]},
	       {makeFiniteWeight[{-1/2,3/2}],makeFiniteWeight[{1/2,-3/2}]},
	       {makeFiniteWeight[{-3/2,1/2}],makeFiniteWeight[{-1/2,-3/2}]},
	       {makeFiniteWeight[{-3/2,-1/2}]}}]]

Module[{b2=makeSimpleRootSystem[B,2]},
       Expect["orbit is equivalent to partial orbit",True, partialOrbit[b2][{rho[b2]}]== orbit[b2][rho[b2]]]]

Module[{b2=makeSimpleRootSystem[B,2]},
       Expect["Positive roots of B2",True, positiveRoots[b2]=={makeFiniteWeight[{1,-1}],makeFiniteWeight[{0,1}],
							       makeFiniteWeight[{1,0}],makeFiniteWeight[{1,1}]}]]

Expect["Dimension",5, dimension[{makeFiniteWeight[{1,1}]}][makeFiniteWeight[{2,2}]]]

Expect["Dimension",5, dimension[makeSimpleRootSystem[A,1]][makeFiniteWeight[{2}]]]

Module[{b2=makeSimpleRootSystem[B,2]},
       Expect["Weights of [2,1] module of B2",True, weightSystem[b2][makeFiniteWeight[{2,1}]]==
	      {{makeFiniteWeight[{2,1}]},
	       {makeFiniteWeight[{1,0}],makeFiniteWeight[{1,1}],makeFiniteWeight[{2,0}]},
	       {makeFiniteWeight[{0,0}]}}]]

Module[{b2=makeSimpleRootSystem[B,2],fm},
       fm=freudenthalMultiplicities[b2][makeFiniteWeight[{2,1}]];
       Expect["Mutliplicites of [2,1] B2 representation",{1, 1, 2, 3, 3},
	      fm[multiplicities]]]

(* Expect["orbitWithEps __TODO__",False,True] *)

Module[{b2=makeSimpleRootSystem[B,2],fm,rm},
       fm=freudenthalMultiplicities[b2][makeFiniteWeight[{2,1}]];
       rm=freudenthalMultiplicities[b2][makeFiniteWeight[{2,1}]];
       Expect["Racah and Freudenthal formulae should give the same result",rm[multiplicities],
	      fm[multiplicities]]]

Expect["Highest root for B2",makeFiniteWeight[{1, 1}],highestRoot[makeSimpleRootSystem[B,2]]]

Expect["simpleRoots of affine Lie algebra A1",True, OverHat[Subscript[A,1]][simpleRoot][1]==makeAffineWeight[{1},0,0]]

Module[{b2a=makeAffineExtension[makeSimpleRootSystem[B,2]]},
       b2a[gradeLimit]=1;
       Expect["Positive roots of affine B2",True, positiveRoots[b2a]=={makeAffineWeight[{-1, -1}, 0, 1], 
								       makeAffineWeight[{1, -1}, 0, 0], 
								       makeAffineWeight[{0, 1}, 0, 0], 
								       makeAffineWeight[{1, 1}, 0, 0], 
								       makeAffineWeight[{1, 0}, 0, 0], 
								       makeAffineWeight[{0, 0}, 0, 0], 
								       makeAffineWeight[{0, 0}, 0, 1], 
								       makeAffineWeight[{0, 0}, 0, 0], 
								       makeAffineWeight[{0, 0}, 0, 1]}];
       b2a[gradeLimit]=.;]



Expect["To main Weyl chamber for affine B2", True,Module[{b2a=OverHat[Subscript[B,2]]}, toFundamentalChamber[b2a][weight[b2a][1,-1,1]]==weight[b2a][0,0,1]]]


Expect["Marks for affine A_3", {1,1,1,1},marks[OverHat[Subscript[A,3]]]]

Expect["Marks for affine C_4", {1, 2, 2, 2, 1},marks[OverHat[Subscript[C,4]]]]


Expect["Comarks for affine D_4", {1, 1, 2, 1, 1}, comarks[OverHat[Subscript[D,4]]]]


Expect["weight of B_2", True,makeFiniteWeight[{3,1}]==weight[makeSimpleRootSystem[B,2]][2,2]]


Expect["Dynkin labels of sl(3) root", True, Module[{rs=makeSimpleRootSystem[A,2]},dynkinLabels[rs][rs[simpleRoot][1]]=={2,-1}]]

Expect["Dynkin labels of so(5) root", True, Module[{rs=makeSimpleRootSystem[B,2]},dynkinLabels[rs][rs[simpleRoot][1]]=={2,-2}]]


Expect["Subsistem, orthogonal to highest root of so(5)", True, Module[{b2=makeSimpleRootSystem[B,2],a1},a1=makeFiniteRootSystem[{highestRoot[b2]}]; orthogonalSubsystem[b2,a1]==makeFiniteRootSystem[{b2[simpleRoot][1]}]]]



Expect["Projection to A_1 \\subset B_2",True, 
       Module[{b2=makeSimpleRootSystem[B,2],a1},
	      a1=makeFiniteRootSystem[{highestRoot[b2]}]; 
	      projection[a1][rho[b2]]==a1[simpleRoot][1]]]


Expect["Projection to A_1 \\subset B_2 for affine algebras",True, 
       Module[{b2a=makeAffineExtension[ makeSimpleRootSystem[B,2]],a1a},
	      a1a=makeAffineExtension[makeFiniteRootSystem[{highestRoot[b2]}]]; 
	      projection[a1][rho[b2a]]==a1[simpleRoot][1] && 
	      projection[a1][b2a[imaginaryRoot]]==a1a[imaginaryRoot]]]


Expect["Formal element construction", 2, makeFormalElement[{makeFiniteWeight[{1,2}],makeFiniteWeight[{3,4}]},{2,3}][makeFiniteWeight[{1,2}]]]


Expect["Formal element construction", 2, makeFormalElement[{makeFiniteWeight[{1,2}],makeFiniteWeight[{3,4}], makeFiniteWeight[{1,2}]}][makeFiniteWeight[{1,2}]]]


Expect["Formal element construction", True, 
       makeFormalElement[{makeFiniteWeight[{1,2}],makeFiniteWeight[{3,4}]},{2,3}]==
       makeFormalElement[makeHashtable[{makeFiniteWeight[{1,2}],makeFiniteWeight[{3,4}]},{2,3}]]]

Expect["subElement",{1},subElement[makeFormalElement[positiveRoots[makeSimpleRootSystem[B,2]],{1,2,3,4}],{makeFiniteWeight[{1,-1}]}][multiplicities]]

Expect["Formal element to hastable conversion", True, 
       Module[{fe=makeFormalElement[{makeFiniteWeight[{1,2}],makeFiniteWeight[{3,4}]},{2,3}]},
	      keys[fe[hashtable]]==fe[weights] && values[fe[hashtable]]==fe[multiplicities]]]


Expect["Formal element addition", 5, 
       (makeFormalElement[{makeFiniteWeight[{1,2}],makeFiniteWeight[{3,4}]},{2,3}]+makeFormalElement[{makeFiniteWeight[{1,2}],makeFiniteWeight[{3,5}]},{3,3}])[makeFiniteWeight[{1,2}]]]

Expect["Formal element multiplication by number", 6, 
       (3*makeFormalElement[{makeFiniteWeight[{1,2}],makeFiniteWeight[{3,4}]},{2,3}])[makeFiniteWeight[{1,2}]]]

Expect["Formal element multiplication by exponent of weight", 2, 
       (Exp[makeFiniteWeight[{1,1}]]*makeFormalElement[{makeFiniteWeight[{1,2}],makeFiniteWeight[{3,4}]},{2,3}])[makeFiniteWeight[{2,3}]]]

Expect["Formal elements multiplication", True,
       Exp[makeFiniteWeight[{1,1}]]*makeFormalElement[{makeFiniteWeight[{1,2}],makeFiniteWeight[{3,4}]},{2,3}]==
       makeFormalElement[{makeFiniteWeight[{1,1}]}]*makeFormalElement[{makeFiniteWeight[{1,2}],makeFiniteWeight[{3,4}]},{2,3}]]

Expect["Projection for formal elements", 2, 
       Module[{b2=makeSimpleRootSystem[B,2],a1},
	      a1=makeFiniteRootSystem[{highestRoot[b2]}];
	      makeFormalElement[projection[a1]/@ Flatten[orbit[b2][weight[b2][1,1]]]][makeFiniteWeight[{1,1}]]]]

Expect["Regular subalgebra B2 of B4", True, regularSubalgebra[makeSimpleRootSystem[B,4]][3,4]==makeFiniteRootSystem[{makeFiniteWeight[{0,0,1,-1}],makeFiniteWeight[{0,0,0,1}]}]]

Expect["Simple branching", True, 
       Module[{b4=makeSimpleRootSystem[B,4],b2,wg},
	      b2=regularSubalgebra[b4][3,4];
	      wg=weight[b4][0,1,0,2];
	      Sort[simpleBranching[b4,b2][wg][multiplicities]]=={6,10,19,30,40,60}]]

Expect["Our branching", True, 
       Module[{b4=makeSimpleRootSystem[B,4],b2,wg},
	      b2=regularSubalgebra[b4][3,4];
	      wg=weight[b4][0,1,0,2];
(*	      Print[wg];*)
	      Union[ourBranching[b4,b2][wg][multiplicities]]=={0,6,10,19,30,40,60}]]

Expect["branching2", True, 
       Module[{b4=makeSimpleRootSystem[B,4],b2,wg,fe, mcw},
	      b2=regularSubalgebra[b4][3,4];
	      wg=weight[b4][0,1,0,2];
	      fe=branching2[b4,b2][wg];
	      mcw=Select[fe[weights],mainChamberQ[b2]];
(*	      Print[fe/@mcw];*)
	      Union[fe/@mcw]=={0,6,10,19,30,40,60}]]

(*

a1=makeAffineExtension[makeSimpleRootSystem[A,1]]

a1[gradeLimit]=40

stringFunction[a1][weight[a1][1,0],weight[a1][1,0]]


a2=makeAffineExtension[makeSimpleRootSystem[A,2]]


stringFunction[a2][weight[a2][1,0,0],weight[a2][1,0,0]]

                                                   2                      3
Out[6]= 1 + 2 Affine`Private`q + 5 Affine`Private`q  + 10 Affine`Private`q  + 
 
                        4                      5                      6
>    20 Affine`Private`q  + 36 Affine`Private`q  + 65 Affine`Private`q  + 
 
                         7                       8                       9
>    110 Affine`Private`q  + 185 Affine`Private`q  + 300 Affine`Private`q
 *)

Print["All tests completed"]