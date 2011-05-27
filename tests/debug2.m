AppendTo[$Path,$InitialDirectory <> "/../src/"];

Mathematica 8.0 for Linux x86 (64-bit)
Copyright 1988-2010 Wolfram Research, Inc.

<<affine.m;

b4=makeSimpleRootSystem[B,10]

pr=positiveRoots[b4];

Length[pr]

Out[5]= 100

[Calculating...]

rho[b4]



                          19  17  15  13  11  9  7  5  3  1
Out[6]= finiteWeight[10, {--, --, --, --, --, -, -, -, -, -}]
                          2   2   2   2   2   2  2  2  2  2

Out[8]= $Aborted

                         15  13  11  9  7  5  3  1
Out[6]= finiteWeight[8, {--, --, --, -, -, -, -, -}]
                         2   2   2   2  2  2  2  2

                         7  5  3  1
Out[4]= finiteWeight[4, {-, -, -, -}]
                         2  2  2  2

Out[3]= finiteRootSystem[4, 4, {finiteWeight[4, {1, -1, 0, 0}], 
 
>     finiteWeight[4, {0, 1, -1, 0}], finiteWeight[4, {0, 0, 1, -1}], 
 
>     finiteWeight[4, {0, 0, 0, 1}]}]

Dot::dotsh: Tensors {1, 2} and {3, 2, 1} have incompatible shapes.

Throw::nocatch: 
   Uncaught Throw[We compare dimensions of vectors before product calculation,
      the expression is left unevaluated in case of dimension mismatch : GOT
      UNEXPECTED VALUE False INSTEAD OF True, ass<<13>>ion] returned to top
     level.

Out[2]= Hold[Throw[We compare dimensions of vectors before product\
 
>      calculation, the expression is left unevaluated in case of dimension\
 
>      mismatch : GOT UNEXPECTED VALUE False INSTEAD OF True, 
 
>     assertion exception]]

Thread::tdlen: Objects of unequal length in {1, 2} + {3, 2, 1}
     cannot be combined.

Throw::nocatch: 
   Uncaught Throw[We compare dimensions of vectors before sum calculation, the
      expression is left unevaluated in case of dimension mismatch : GOT
      UNEXPECTED VALUE False INSTEAD OF True, assert<<10>>ion] returned to top
     level.

Out[2]= Hold[Throw[We compare dimensions of vectors before sum calculation,\
 
>      the expression is left unevaluated in case of dimension mismatch : GOT\
 
>      UNEXPECTED VALUE False INSTEAD OF True, assertion exception]]

Thread::tdlen: Objects of unequal length in {1, 2, 3} + {3, 2, 1, 2}
     cannot be combined.

Throw::nocatch: 
   Uncaught Throw[Plus product for vectors from different spaces are left
      unevaluated: GOT UNEXPECTED VALUE False INSTEAD OF True, 
     assertion exception] returned to top level.

Out[2]= Hold[Throw[Plus product for vectors from different spaces are left\
 
>      unevaluated: GOT UNEXPECTED VALUE False INSTEAD OF True, 
 
>     assertion exception]]

Dot::dotsh: Tensors {1, 2, 3} and {3, 2, 1, 2} have incompatible shapes.

Throw::nocatch: 
   Uncaught Throw[Scalar product for vectors from different spaces are left
      unevaluated: GOT UNEXPECTED VALUE False INSTEAD OF True, 
     assertion exception] returned to top level.

Out[2]= Hold[Throw[Scalar product for vectors from different spaces are left\
 
>      unevaluated: GOT UNEXPECTED VALUE False INSTEAD OF True, 
 
>     assertion exception]]

{1,2,3}.{2,3}

{1,2,3}+{2,3}

Thread::tdlen: Objects of unequal length in {1, 2, 3} + {2, 3}
     cannot be combined.

Out[4]= {2, 3} + {1, 2, 3}

Dot::dotsh: Tensors {1, 2, 3} and {2, 3} have incompatible shapes.

Out[3]= {1, 2, 3} . {2, 3}

Module::lvsym: Local variable specification 
    {b4 = makeSimpleRootSystem[B, 4], b2, wg, fe . mcw} contains fe . mcw
    , which is not a symbol or an assignment to a symbol.

Expect["Our branching", True, 
       Module[{b4=makeSimpleRootSystem[B,4],b2,wg,fe},
	      b2=regularSubalgebra[b4][3,4];
	      wg=weight[b4][0,1,0,2];
	      fe=ourBranching[b4,b2][wg];
	      Export["br1.png",drawPlaneProjection[4,3,fe]];
	      fe=simpleBranching[b4,b2][wg];
	      Export["br2.png",drawPlaneProjection[4,3,fe]];
	      fe=branching2[b4,b2][wg];
	      Export["br3.png",drawPlaneProjection[4,3,fe]];
	      True]]

                                                                                
Set::write: Tag Times in br2.png formalElement[table$2727] is Protected.

Out[4]= If[True != br3.png True, 
 
>    Throw[ToString[Our branching]<>: GOT UNEXPECTED VALUE <>
 
>      ToString[br3.png True]<> INSTEAD OF <>ToString[True], 
 
>     assertion exception]]


Expect["branching2", True, 


       Module[{b4=makeSimpleRootSystem[B,4],b2,wg},
	      b2=regularSubalgebra[b4][3,4];
	      wg=weight[b4][0,1,0,2];
	      branching2[b4,b2][wg][multiplicities]]

       Module[{b4=makeSimpleRootSystem[B,4],b2,wg,fe,mcw},
	      b2=regularSubalgebra[b4][3,4];
	      wg=weight[b4][0,1,0,2];
	      fe=branching2[b4,b2][wg];
	      mcw=Select[fe[weights],mainChamberQ[b2]];
	      Union[fe/@mcw]]

                                             
Out[10]= {0, 6, 10, 19, 30, 40, 60}

                                        
Union::normal: Nonatomic expression expected at position 1 in Union[mcs].

Out[9]= Union[mcs]

                                        
Module::lvsym: Local variable specification 
    {b4 = makeSimpleRootSystem[B, 4], b2, wg, fe . mcw} contains fe . mcw
    , which is not a symbol or an assignment to a symbol.

Out[8]= Module[{b4 = makeSimpleRootSystem[B, 4], b2, wg, fe . mcw}, 
 
>    b2 = regularSubalgebra[b4][3, 4]; wg = weight[b4][0, 1, 0, 2]; 
 
>     fe = branching2[b4, b2][wg]; 
 
>     mcw = Select[fe[weights], mainChamberQ[b2]]; Union[fe /@ mcs]]


                        
Out[7]= {0, 0, 0, 0, 0, 0, 19, 10, -60, 30, 0, 60, 0, 0, 0, -60, -10, 0, 0, 
 
>    0, 0, 0, -6, -19, -60, 60, 0, -19, 19, 0, -6, 0, 60, 6, 0, 10, 6, 0, 0, 
 
>    -10, -19, -6, 0, 10, -60, 6, 0, 40, 40, -6, 30, 0, -40, 30, 0, 0, 0, 0, 
 
>    40, 0, -10, 0, 0, 0, -30, -30, 0, 0, 0, 0, -40, 19, 0, 30, 0, -40, 0, 
 
>    -30, 6, -30, 60}

                                
Throw::nocatch: 
   Uncaught Throw[branching2: GOT UNEXPECTED VALUE False INSTEAD OF True, 
     assertion exception] returned to top level.

Out[6]= Hold[Throw[branching2: GOT UNEXPECTED VALUE False INSTEAD OF True, 
 
>     assertion exception]]


Expect["Formal element to hastable conversion", True, 
       Module[{fe=makeFormalElement[{makeFiniteWeight[{1,2}],makeFiniteWeight[{3,4}]},{2,3}]},
	      keys[fe[hashtable]]==fe[weights] && values[fe[hashtable]]==fe[multiplicities]]]

Expect["Formal element addition", 5, 
       (makeFormalElement[{makeFiniteWeight[{1,2}],makeFiniteWeight[{3,4}]},{2,3}]+makeFormalElement[{makeFiniteWeight[{1,2}],makeFiniteWeight[{3,5}]},{3,3}])[makeFiniteWeight[{1,2}]]]

Expect["Formal element multiplication by number", 6, 
       (3*makeFormalElement[{makeFiniteWeight[{1,2}],makeFiniteWeight[{3,4}]},{2,3}])[makeFiniteWeight[{1,2}]]]

Expect["Formal element multiplication by exponent of weight", 2, 
       (Exp[makeFiniteWeight[{1,1}]]*makeFormalElement[{makeFiniteWeight[{1,2}],makeFiniteWeight[{3,4}]},{2,3}])[makeFiniteWeight[{2,3}]]]

Square[makeFormalElement[{makeFiniteWeight[{1,2}],makeFiniteWeight[{3,4}]},{2,3}]]

Expect["Formal elements multiplication", True,
       Exp[makeFiniteWeight[{1,1}]]*makeFormalElement[{makeFiniteWeight[{1,2}],makeFiniteWeight[{3,4}]},{2,3}]==
       makeFormalElement[{makeFiniteWeight[{1,1}]}]*makeFormalElement[{makeFiniteWeight[{1,2}],makeFiniteWeight[{3,4}]},{2,3}]]

projection[a1]/@ orbit[b2][weight[b2][1,1]]

Module[{b2=makeSimpleRootSystem[B,2],a1},
       a1=makeFiniteRootSystem[{highestRoot[b2]}];
       projection[a1]/@ orbit[b2][weight[b2][1,1]]]

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

Expect["Weyl vector for B2",True,makeFiniteWeight[{3/2,1/2}]==rho[positiveRoots[makeSimpleRootSystem[B,2]]]]

Expect["Our branching", True]


Module[{b4=makeSimpleRootSystem[B,4],b2,wg},
       b2=regularSubalgebra[b4][3,4];
       wg=weight[b4][0,1,0,2];
       Sort[ourBranching[b4,b2][wg][multiplicities]]]

[Calculating...]


                          
Throw::nocatch: 
   Uncaught Throw[Projection for formal elements: GOT UNEXPECTED VALUE 0
      INSTEAD OF 2, assertion exception] returned to top level.

Out[17]= Hold[Throw[Projection for formal elements: GOT UNEXPECTED VALUE 0\
 
>      INSTEAD OF 2, assertion exception]]


                  
Out[16]= {{finiteWeight[2, {1, 1}]}, 
 
                                                1  1
>    {finiteWeight[2, {1, 1}], finiteWeight[2, {-, -}]}, 
                                                2  2
 
                       1  1                       1     1
>    {finiteWeight[2, {-, -}], finiteWeight[2, {-(-), -(-)}]}, 
                       2  2                       2     2
 
                         1     1
>    {finiteWeight[2, {-(-), -(-)}], finiteWeight[2, {-1, -1}]}, 
                         2     2
 
>    {finiteWeight[2, {-1, -1}]}}


Out[15]= orbit[b2][projection[a1][weight[b2][1, 1]]]


Out[12]= []formalElement[table$187]

                                 2
Out[11]= formalElement[table$186]






Expect["Formal element construction", 2, makeFormalElement[{makeFiniteWeight[{1,2}],makeFiniteWeight[{3,4}]},{2,3}][makeFiniteWeight[{1,2}]]]

Expect["Formal element construction", 2, makeFormalElement[{makeFiniteWeight[{1,2}],makeFiniteWeight[{3,4}], makeFiniteWeight[{1,2}]}][makeFiniteWeight[{1,2}]]]

Expect["Formal element construction", True, makeFormalElement[{makeFiniteWeight[{1,2}],makeFiniteWeight[{3,4}]},{2,3}]==makeFormalElement[makeHashtable[{makeFiniteWeight[{1,2}],makeFiniteWeight[{3,4}]},{2,3}]]]

Out[5]= If[True != (formalElement[table$129] == formalElement[table$130]), 
 
>    Throw[ToString[Formal element construction]<>: GOT UNEXPECTED VALUE <>
 
>      ToString[formalElement[table$129] == formalElement[table$130]]<>
 
>       INSTEAD OF <>ToString[True], assertion exception]]



Throw::nocatch: 
   Uncaught Throw[Dynkin labels of sl(3) root: GOT UNEXPECTED VALUE False
      INSTEAD OF True, assertion exception] returned to top level.

Out[45]= Hold[Throw[Dynkin labels of sl(3) root: GOT UNEXPECTED VALUE False\
 
>      INSTEAD OF True, assertion exception]]

Module[{rs=makeSimpleRootSystem[B,2]},dynkinLabels[rs][rs[simpleRoot][1]]=={2,-2}]

dynkinLabels[makeSimpleRootSystem[B,2]][makeSimpleRootSystem[B,2][simpleRoot][1]]

Out[50]= {2, -2}

Out[46]= {1, -1}

fundamentalWeights[makeSimpleRootSystem[B,2]]

makeSimpleRootSystem[B,2][simpleRoots]

Out[44]= {finiteWeight[2, {1, -1}], finiteWeight[2, {0, 1}]}

                                                    1  1
Out[43]= {finiteWeight[2, {1, 0}], finiteWeight[2, {-, -}]}
                                                    2  2

Out[42]= {2, -1}

Out[41]= {2, 0, -1}

Out[40]= False

Throw::nocatch: 
   Uncaught Throw[Dynkin labels of so(5) root: GOT UNEXPECTED VALUE False
      INSTEAD OF True, assertion exception] returned to top level.

Out[39]= Hold[Throw[Dynkin labels of so(5) root: GOT UNEXPECTED VALUE False\
 
>      INSTEAD OF True, assertion exception]]

b4=Subscript[B,4];
al=b4[simpleRoots];
b={al[[3]],al[[4]]};
b2=makeFiniteRootSystem[b];

b2=makeAffineExtension[makeSimpleRootSystem[B,2]];

weylGroupElement[b2][0,0,0] @ makeAffineWeight[{1,2},1,1]

Out[15]= affineWeight[2, finiteWeight[2, {-1, 0}], 1, 3]

Out[14]= affineWeight[2, finiteWeight[2, {2, 1}], 1, 1]

weylGroupElement[b2][0,1,0] @ makeAffineWeight[{1,2},1,1]

Out[13]= affineWeight[2, finiteWeight[2, {-1, 0}], 1, 3]

Out[12]= affineWeight[2, finiteWeight[2, {1, 2}], 1, 1]

b2[simpleRoot][0]

Expect["Comarks for affine D_4", {1, 1, 2, 1, 1}, comarks[OverHat[Subscript[D,4]]]]

Expect["Dynkin labels of sl(3) root", True, Module[{rs=makeSimpleRootSystem[A,2]},dynkinLabels[rs][rs[simpleRoot][1]]==[2,-1]]]

Expect["Dynkin labels of sl(3) root", True, Module[{rs=makeSimpleRootSystem[A,2]},dynkinLabels[rs][rs[simpleRoot][1]]=={2,-1}]]

Expect["Dynkin labels of so(5) root", True, Module[{rs=makeSimpleRootSystem[B,2]},dynkinLabels[rs][rs[simpleRoot][1]]=={2,-2}]]

Expect["Subsistem, orthogonal to highest root of so(5)", True, Module[{b2=makeSimpleRootSystem[B,2],a1},a1=makeFiniteRootSystem[{highestRoot[b2]}]; orthogonalSubsystem[b2,a1]==makeFiniteRootSystem[{b2[simpleRoot][1]}]]]

Expect["Projection to A_1 \\subset B_2",True, 
       Module[{b2=makeSimpleRootSystem[B,2],a1},
	      a1=makeFiniteRootSystem[{highestRoot[b2]}]; 
	      projection[a1][rho[b2]]==2*a1[simpleRoot][1]]]

Expect["Projection to A_1 \\subset B_2 for affine algebras",True, 
       Module[{b2a=makeAffineExtension[ makeSimpleRootSystem[B,2]],a1a},
	      a1a=makeAffineExtension[makeFiniteRootSystem[{highestRoot[b2]}]]; 
	      projection[a1][rho[b2a]]==a1[simpleRoot][1]]]

Expect["Projection to A_1 \\subset B_2 for affine algebras",True, 
       Module[{b2a=makeAffineExtension[ makeSimpleRootSystem[B,2]],a1a},
	      a1a=makeAffineExtension[makeFiniteRootSystem[{highestRoot[b2a]}]]; 
	      Print[projection[a1a][rho[b2a]]];
	      Print[a1a[simpleRoot][1]];True]]

Out[16]= affineWeight[2, finiteWeight[2, {-1, -1}], 0, 1]

Out[11]= affineWeight[2, finiteWeight[2, {1, -1}], 0, 0]

Out[10]= affineWeight[2, finiteWeight[2, {0, -1}], 1, 3]

Out[9]= affineWeight[2, finiteWeight[2, {1, 2}], 1, 1]

Out[8]= affineWeight[2, finiteWeight[2, {-1, 0}], 1, 3]

Out[7]= affineWeight[2, finiteWeight[2, {1, 2}], 1, 1]

Out[6]= affineWeight[2, finiteWeight[2, {1, 1}], 1, 1]

Out[5]= affineWeight[2, finiteWeight[2, {1, 1}], 1, 1]

Out[4]= Function[z$, Fold[revApply, z$, 
 
>     reflection /@ 
 
>      affineRootSystem[2, finiteRootSystem[2, 2, 
 
>          {finiteWeight[2, {1, -1}], finiteWeight[2, {0, 1}]}], 
 
>         affineWeight[2, finiteWeight[2, {-1, -1}], 0, 1], 
 
>         {affineWeight[2, finiteWeight[2, {1, -1}], 0, 0], 
 
>          affineWeight[2, finiteWeight[2, {0, 1}], 0, 0]}][simpleRoot] /@ 
 
>       {0, 1}]]

toFundamentalChamber[rs_?rootSystemQ][vec_?weightQ]:=
    NestWhile[Function[v,
			     reflection[Scan[If[#.v<0,Return[#]]&,rs[simpleRoots]]][v]],
		    vec,
		    Head[#]=!=reflection[Null]&]

?NestWhile

toFundamentalChamberWithParity[rs_?rootSystemQ][vec_?weightQ]:=
    ({#[[1]][[1]],-#[[2]]})& @ NestWhile[Function[v,
			     {reflection[Scan[If[#.v[[1]]<0,Return[#]]&,rs[simpleRoots]]][v[[1]]],-v[[2]]}],
		    {vec,1},
		    Head[#[[1]]]=!=reflection[Null]&]


tmp=toFundamentalChamberWithParity[b2][makeFiniteWeight[{0,0,1,1}]]


Out[15]= reflection[Null][finiteWeight[4, {0, 0, 1, 1}]]

toFundamentalChamber[b2][makeFiniteWeight[{0,0,-1,-1}]]


finiteWeight[4, {0, 0, -1, -1}]
finiteWeight[4, {0, 0, -1, 1}]
finiteWeight[4, {0, 0, 1, -1}]
finiteWeight[4, {0, 0, 1, 1}]

Out[12]= finiteWeight[4, {0, 0, 1, 1}]

f=fan[b4,b2];

Export["/home/anton/programing/Affine/tests/fan.png",Graphics[Text[f[#],{#[standardBase][[4]],#[standardBase][[3]]}] & /@ f[weights]]]

Out[5]= /home/anton/programing/Affine/tests/fan.png

wg=weight[b4][0,1,0,2]; (* makeFiniteWeight[{2,2,1,1}] *)

aw=projection[b2][anomalousWeights[b4][wg]];

Export["/home/anton/programing/Affine/tests/anom.png",Graphics[Text[aw[#],{#[standardBase][[4]],#[standardBase][[3]]}] & /@ aw[weights]]]

extendedAnomElement[rs_?rootSystemQ,subs_?rootSystemQ][highestWeight_?weightQ]:=
    Module[{anomW,selW,selWM,rh=rho[rs],orth,ortrh},
	   orth=orthogonalSubsystem[rs,subs];
	   ortrh=rho[orth];
	   anomW=anomalousWeights[rs][highestWeight];
	   selW=Select[anomW[weights],Function[x,mainChamberQ[orth][x+rh-projection[subs][x+rh]]]];
	   selWM=makeFormalElement[projection[subs][selW],(anomW[#]*dimension[orth][#+rh-ortrh])&/@selW];
	   selWM];


eae=extendedAnomElement[b4,b2][wg];

Export["/home/anton/programing/Affine/tests/eanom.png",Graphics[Text[eae[#],{#[standardBase][[4]],#[standardBase][[3]]}] & /@ eae[weights],Axes->True]]


rh=rho[b4]

def=projection[b2][{rh}][[1]]

[Calculating...]

Out[41]= finiteWeight[4, {0, 0, 0, 0}]

                             7     5
Out[36]= {finiteWeight[4, {-(-), -(-), 0, 0}]}
                             2     2

                                       3  1
Out[34]= {-rh + finiteWeight[4, {0, 0, -, -}]}
                                       2  2

Out[33]= /home/anton/programing/Affine/tests/eanom.png

subrh=rho[b2]

                                3  1
Out[22]= finiteWeight[4, {0, 0, -, -}]
                                2  2

hw=Sort[eae[weights],#1.subrh<#2.subrh&][[1]]

Out[14]= finiteWeight[4, {0, 0, -4, 4}]

Out[35]= finiteWeight[4, {0, 0, 4, 4}]

subs=b2

Sort[
    Flatten[orbit[b2][getOrderedWeightsProjectedToWeylChamber[positiveRoots[b4],b2,finiteWeight[4, {0, 0, 4, 4}]]]],
    #1.subrh<#2.subrh]

                  
Out[23]= {finiteWeight[4, {0, 0, 4, 4}], finiteWeight[4, {0, 0, 4, 3}], 
 
>    finiteWeight[4, {0, 0, 4, 2}], finiteWeight[4, {0, 0, 4, 1}], 
 
>    finiteWeight[4, {0, 0, 4, 0}], finiteWeight[4, {0, 0, 3, 3}], 
 
>    finiteWeight[4, {0, 0, 3, 2}], finiteWeight[4, {0, 0, 3, 1}], 
 
>    finiteWeight[4, {0, 0, 3, 0}], finiteWeight[4, {0, 0, 2, 2}], 
 
>    finiteWeight[4, {0, 0, 2, 1}], finiteWeight[4, {0, 0, 2, 0}], 
 
>    finiteWeight[4, {0, 0, 1, 1}], finiteWeight[4, {0, 0, 1, 0}], 
 
>    finiteWeight[4, {0, 0, 0, 0}], finiteWeight[4, {0, 0, 0, 1}], 
 
>    finiteWeight[4, {0, 0, 0, 2}], finiteWeight[4, {0, 0, 0, 3}], 
 
>    finiteWeight[4, {0, 0, 0, 4}], finiteWeight[4, {0, 0, 1, -1}], 
 
>    finiteWeight[4, {0, 0, 1, 2}], finiteWeight[4, {0, 0, 1, 3}], 
 
>    finiteWeight[4, {0, 0, 1, 4}], finiteWeight[4, {0, 0, 2, -2}], 
 
>    finiteWeight[4, {0, 0, 2, -1}], finiteWeight[4, {0, 0, 2, 3}], 
 
>    finiteWeight[4, {0, 0, 2, 4}], finiteWeight[4, {0, 0, 3, -3}], 
 
>    finiteWeight[4, {0, 0, 3, -2}], finiteWeight[4, {0, 0, 3, -1}], 
 
>    finiteWeight[4, {0, 0, 3, 4}], finiteWeight[4, {0, 0, 4, -4}], 
 
>    finiteWeight[4, {0, 0, 4, -3}], finiteWeight[4, {0, 0, 4, -2}], 
 
>    finiteWeight[4, {0, 0, 4, -1}], finiteWeight[4, {0, 0, -4, 4}], 
 
>    finiteWeight[4, {0, 0, -3, 3}], finiteWeight[4, {0, 0, -3, 4}], 
 
>    finiteWeight[4, {0, 0, -2, 2}], finiteWeight[4, {0, 0, -2, 3}], 
 
>    finiteWeight[4, {0, 0, -2, 4}], finiteWeight[4, {0, 0, -1, 1}], 
 
>    finiteWeight[4, {0, 0, -1, 2}], finiteWeight[4, {0, 0, -1, 3}], 
 
>    finiteWeight[4, {0, 0, -1, 4}], finiteWeight[4, {0, 0, 0, -4}], 
 
>    finiteWeight[4, {0, 0, 0, -3}], finiteWeight[4, {0, 0, 0, -2}], 
 
>    finiteWeight[4, {0, 0, 0, -1}], finiteWeight[4, {0, 0, 1, -4}], 
 
>    finiteWeight[4, {0, 0, 1, -3}], finiteWeight[4, {0, 0, 1, -2}], 
 
>    finiteWeight[4, {0, 0, 2, -4}], finiteWeight[4, {0, 0, 2, -3}], 
 
>    finiteWeight[4, {0, 0, 3, -4}], finiteWeight[4, {0, 0, -4, -4}], 
 
>    finiteWeight[4, {0, 0, -4, 0}], finiteWeight[4, {0, 0, -4, 1}], 
 
>    finiteWeight[4, {0, 0, -4, 2}], finiteWeight[4, {0, 0, -4, 3}], 
 
>    finiteWeight[4, {0, 0, -3, -4}], finiteWeight[4, {0, 0, -3, -3}], 
 
>    finiteWeight[4, {0, 0, -3, 0}], finiteWeight[4, {0, 0, -3, 1}], 
 
>    finiteWeight[4, {0, 0, -3, 2}], finiteWeight[4, {0, 0, -2, -4}], 
 
>    finiteWeight[4, {0, 0, -2, -3}], finiteWeight[4, {0, 0, -2, -2}], 
 
>    finiteWeight[4, {0, 0, -2, 0}], finiteWeight[4, {0, 0, -2, 1}], 
 
>    finiteWeight[4, {0, 0, -1, -4}], finiteWeight[4, {0, 0, -1, -3}], 
 
>    finiteWeight[4, {0, 0, -1, -2}], finiteWeight[4, {0, 0, -1, -1}], 
 
>    finiteWeight[4, {0, 0, -1, 0}], finiteWeight[4, {0, 0, -4, -3}], 
 
>    finiteWeight[4, {0, 0, -4, -2}], finiteWeight[4, {0, 0, -4, -1}], 
 
>    finiteWeight[4, {0, 0, -3, -2}], finiteWeight[4, {0, 0, -3, -1}], 
 
>    finiteWeight[4, {0, 0, -2, -1}]}


Union[getOrderedWeightsProjectedToWeylChamber[positiveRoots[b4],subs,hw]]

projection[subs][positiveRoots[b4]]

Select[projection[subs][positiveRoots[b4]],(#=!=zeroWeight[subs])& ]

weightSystem[Select[projection[subs][positiveRoots[b4]],(#=!=zeroWeight[subs] && #.subrh)& ]][projection[subs][{hw}][[1]]]

Out[45]= {{finiteWeight[4, {0, 0, 4, 4}]}}

Out[44]= weightSystem[Select[projection[subs][positiveRoots[b4]], 
 
>       #1 =!= finiteWeight[4, {0, 0, 0, 0}]] & ][finiteWeight[4, 
 
>     {0, 0, 4, 4}]]

Out[43]= weightSystem[{}][finiteWeight[4, {0, 0, 4, 4}]]

projection[subs][{hw}][[1]]

Out[42]= finiteWeight[4, {0, 0, 4, 4}]

Out[40]= {{finiteWeight[4, {0, 0, 4, 4}]}}


Out[36]= {finiteWeight[4, {0, 0, 4, 4}]}

Out[31]= finiteWeight[4, {0, 0, 4, 4}]

Out[30]= finiteWeight[4, {0, 0, -7, -5}]

Out[28]= /home/anton/programing/Affine/tests/eanom.png

Out[9]= /home/anton/programing/Affine/tests/eanom.png

eae[makeFiniteWeight[{0,0,4,4}]]

br=ourBranching[b4,b2][wg]

br1=branching2[b4,b2][wg]



Out[61]= formalElement[table$3776]

Out[53]= formalElement[table$3412]


(makeFormalElement[{makeFiniteWeight[{1,2}],makeFiniteWeight[{2,2}]},{1,2}]*Exp[-makeFiniteWeight[{1,2}]])[weights]

Out[48]= {finiteWeight[2, {0, 0}], finiteWeight[2, {1, 0}]}

Out[47]= formalElement[table$3045]

Export["/home/anton/programing/Affine/tests/brc1.png",Graphics[Text[br1[#],{#[standardBase][[4]],#[standardBase][[3]]}] & /@ br1[weights],Axes->True]]

Out[78]= /home/anton/programing/Affine/tests/brc1.png

br1 /@ Select[br1[weights],mainChamberQ[b2]]

Out[77]= {10, 0, 0, -194, 19, 0, 0, 0, 0, 0, -170, 0, 40, 0, 0}

Out[66]= {0, 0, 0, 0, 0, 139, 0, 0, -2562, -40, 10, 0, 0, 320, 17522}

Out[64]= {-194, 0, -170, 19, 0, 0, 0, 0, 0, 0, 0, 10, 40, 0, 0}

Out[57]= {0, 0, 194, 0, 0, -40, -10, 0, 0, 0, 0, -19, 170, 0, 0}

Out[56]= {finiteWeight[4, {0, 0, 3, 3}], finiteWeight[4, {0, 0, 3, 2}], 
 
>    finiteWeight[4, {0, 0, 0, 0}], finiteWeight[4, {0, 0, 4, 3}], 
 
>    finiteWeight[4, {0, 0, 3, 0}], finiteWeight[4, {0, 0, 2, 1}], 
 
>    finiteWeight[4, {0, 0, 2, 2}], finiteWeight[4, {0, 0, 4, 2}], 
 
>    finiteWeight[4, {0, 0, 3, 1}], finiteWeight[4, {0, 0, 1, 1}], 
 
>    finiteWeight[4, {0, 0, 4, 4}], finiteWeight[4, {0, 0, 2, 0}], 
 
>    finiteWeight[4, {0, 0, 1, 0}], finiteWeight[4, {0, 0, 4, 1}], 
 
>    finiteWeight[4, {0, 0, 4, 0}]}

Out[55]= {}

Out[54]= /home/anton/programing/Affine/tests/brc1.png

Out[28]= /home/anton/programing/Affine/tests/brc1.png

Out[27]= formalElement[table$2264]

10
finiteWeight[4, {0, 0, 2, 1}]
{{finiteWeight[4, {0, 0, 4, 3}], {finiteWeight[4, {0, 0, 4, 3}], 1}, 1, 0}, 
 
>   {finiteWeight[4, {0, 0, 4, 0}], {finiteWeight[4, {0, 0, 4, 0}], 1}, -4, 
 
>    0}, {finiteWeight[4, {0, 0, 3, 1}], {finiteWeight[4, {0, 0, 3, 1}], 1}, 
 
>    -24, 0}, {finiteWeight[4, {0, 0, 3, -1}], 
 
>    {finiteWeight[4, {0, 0, 3, 0}], -1}, -4, 0}, 
 
>   {finiteWeight[4, {0, 0, 2, 0}], {finiteWeight[4, {0, 0, 2, 0}], 1}, -24, 
 
>    -24 table$7784[finiteWeight[4, {0, 0, 2, 0}]]}, 
 
>   {finiteWeight[4, {0, 0, 3, 2}], {finiteWeight[4, {0, 0, 3, 2}], 1}, 16, 
 
>    0}, {finiteWeight[4, {0, 0, 2, 2}], {finiteWeight[4, {0, 0, 2, 2}], 1}, 
 
>    -24, -240}, {finiteWeight[4, {0, 0, 3, 0}], 
 
>    {finiteWeight[4, {0, 0, 3, 0}], 1}, 16, 0}, 
 
>   {finiteWeight[4, {0, 0, 1, 3}], {finiteWeight[4, {0, 0, 2, 2}], -1}, -4, 
 
>    40}, {finiteWeight[4, {0, 0, 0, 2}], 
 
>    {finiteWeight[4, {0, 0, 1, 1}], -1}, -4, 
 
>    4 table$7784[finiteWeight[4, {0, 0, 1, 1}]]}, 
 
>   {finiteWeight[4, {0, 0, 4, 2}], {finiteWeight[4, {0, 0, 4, 2}], 1}, -4, 
 
>    0}, {finiteWeight[4, {0, 0, 3, 3}], {finiteWeight[4, {0, 0, 3, 3}], 1}, 
 
>    -4, 0}, {finiteWeight[4, {0, 0, 0, 3}], 
 
>    {finiteWeight[4, {0, 0, 2, 1}], -1}, 1, 
 
>    -table$7784[finiteWeight[4, {0, 0, 2, 1}]]}, 
 
>   {finiteWeight[4, {0, 0, 1, 1}], {finiteWeight[4, {0, 0, 1, 1}], 1}, -24, 
 
>    -24 table$7784[finiteWeight[4, {0, 0, 1, 1}]]}, 
 
>   {finiteWeight[4, {0, 0, 2, 3}], {finiteWeight[4, {0, 0, 2, 3}], 1}, 6, 
 
>    6 table$7784[finiteWeight[4, {0, 0, 2, 3}]]}, 
 
>   {finiteWeight[4, {0, 0, 2, 1}], {finiteWeight[4, {0, 0, 2, 1}], 1}, 36, 
 
>    36 table$7784[finiteWeight[4, {0, 0, 2, 1}]]}, 
 
>   {finiteWeight[4, {0, 0, 1, 2}], {finiteWeight[4, {0, 0, 1, 2}], 1}, 16, 
 
>    16 table$7784[finiteWeight[4, {0, 0, 1, 2}]]}, 
 
>   {finiteWeight[4, {0, 0, 4, -1}], {finiteWeight[4, {0, 0, 4, 0}], -1}, 1, 
 
>    0}, {finiteWeight[4, {0, 0, 1, 0}], {finiteWeight[4, {0, 0, 1, 0}], 1}, 
 
>    16, 16 table$7784[finiteWeight[4, {0, 0, 1, 0}]]}, 
 
>   {finiteWeight[4, {0, 0, 1, -1}], {finiteWeight[4, {0, 0, 1, 0}], -1}, -4, 
 
>    4 table$7784[finiteWeight[4, {0, 0, 1, 0}]]}, 
 
>   {finiteWeight[4, {0, 0, 0, 0}], {finiteWeight[4, {0, 0, 0, 0}], 1}, -4, 
 
>    -4 table$7784[finiteWeight[4, {0, 0, 0, 0}]]}, 
 
>   {finiteWeight[4, {0, 0, 0, -1}], {finiteWeight[4, {0, 0, 0, 0}], -1}, 1, 
 
>    -table$7784[finiteWeight[4, {0, 0, 0, 0}]]}, 
 
>   {finiteWeight[4, {0, 0, 2, -1}], {finiteWeight[4, {0, 0, 2, 0}], -1}, 6, 
 
>    -6 table$7784[finiteWeight[4, {0, 0, 2, 0}]]}, 
 
>   {finiteWeight[4, {0, 0, 4, 1}], {finiteWeight[4, {0, 0, 4, 1}], 1}, 6, 
 
>    0}, {finiteWeight[4, {0, 0, 0, 1}], {finiteWeight[4, {0, 0, 0, 1}], 1}, 
 
>    6, 6 table$7784[finiteWeight[4, {0, 0, 0, 1}]]}}
200
finiteWeight[4, {0, 0, 2, 0}]
{{finiteWeight[4, {0, 0, 4, 2}], {finiteWeight[4, {0, 0, 4, 2}], 1}, 1, 0}, 
 
>   {finiteWeight[4, {0, 0, 4, -1}], {finiteWeight[4, {0, 0, 4, 0}], -1}, -4, 
 
>    0}, {finiteWeight[4, {0, 0, 3, 0}], {finiteWeight[4, {0, 0, 3, 0}], 1}, 
 
>    -24, 0}, {finiteWeight[4, {0, 0, 3, -2}], 
 
>    {finiteWeight[4, {0, 0, 3, 1}], -1}, -4, 0}, 
 
>   {finiteWeight[4, {0, 0, 2, -1}], {finiteWeight[4, {0, 0, 2, 0}], -1}, 
 
>    -24, 24 table$7784[finiteWeight[4, {0, 0, 2, 0}]]}, 
 
>   {finiteWeight[4, {0, 0, 3, 1}], {finiteWeight[4, {0, 0, 3, 1}], 1}, 16, 
 
>    0}, {finiteWeight[4, {0, 0, 2, 1}], {finiteWeight[4, {0, 0, 2, 1}], 1}, 
 
>    -24, -4800}, {finiteWeight[4, {0, 0, 3, -1}], 
 
>    {finiteWeight[4, {0, 0, 3, 0}], -1}, 16, 0}, 
 
>   {finiteWeight[4, {0, 0, 1, 2}], {finiteWeight[4, {0, 0, 1, 2}], 1}, -4, 
 
>    -4 table$7784[finiteWeight[4, {0, 0, 1, 2}]]}, 
 
>   {finiteWeight[4, {0, 0, 0, 1}], {finiteWeight[4, {0, 0, 0, 1}], 1}, -4, 
 
>    -4 table$7784[finiteWeight[4, {0, 0, 0, 1}]]}, 
 
>   {finiteWeight[4, {0, 0, 4, 1}], {finiteWeight[4, {0, 0, 4, 1}], 1}, -4, 
 
>    0}, {finiteWeight[4, {0, 0, 3, 2}], {finiteWeight[4, {0, 0, 3, 2}], 1}, 
 
>    -4, 0}, {finiteWeight[4, {0, 0, 0, 2}], 
 
>    {finiteWeight[4, {0, 0, 1, 1}], -1}, 1, 
 
>    -table$7784[finiteWeight[4, {0, 0, 1, 1}]]}, 
 
>   {finiteWeight[4, {0, 0, 1, 0}], {finiteWeight[4, {0, 0, 1, 0}], 1}, -24, 
 
>    -24 table$7784[finiteWeight[4, {0, 0, 1, 0}]]}, 
 
>   {finiteWeight[4, {0, 0, 2, 2}], {finiteWeight[4, {0, 0, 2, 2}], 1}, 6, 
 
>    60}, {finiteWeight[4, {0, 0, 2, 0}], {finiteWeight[4, {0, 0, 2, 0}], 1}, 
 
>    36, 36 table$7784[finiteWeight[4, {0, 0, 2, 0}]]}, 
 
>   {finiteWeight[4, {0, 0, 1, 1}], {finiteWeight[4, {0, 0, 1, 1}], 1}, 16, 
 
>    16 table$7784[finiteWeight[4, {0, 0, 1, 1}]]}, 
 
>   {finiteWeight[4, {0, 0, 4, -2}], {finiteWeight[4, {0, 0, 4, 1}], -1}, 1, 
 
>    0}, {finiteWeight[4, {0, 0, 1, -1}], 
 
>    {finiteWeight[4, {0, 0, 1, 0}], -1}, 16, 
 
>    -16 table$7784[finiteWeight[4, {0, 0, 1, 0}]]}, 
 
>   {finiteWeight[4, {0, 0, 1, -2}], {finiteWeight[4, {0, 0, 1, 1}], -1}, -4, 
 
>    4 table$7784[finiteWeight[4, {0, 0, 1, 1}]]}, 
 
>   {finiteWeight[4, {0, 0, 0, -1}], {finiteWeight[4, {0, 0, 0, 0}], -1}, -4, 
 
>    4 table$7784[finiteWeight[4, {0, 0, 0, 0}]]}, 
 
>   {finiteWeight[4, {0, 0, 0, -2}], {finiteWeight[4, {0, 0, 0, 1}], -1}, 1, 
 
>    -table$7784[finiteWeight[4, {0, 0, 0, 1}]]}, 
 
>   {finiteWeight[4, {0, 0, 2, -2}], {finiteWeight[4, {0, 0, 2, 1}], -1}, 6, 
 
>    -1200}, {finiteWeight[4, {0, 0, 4, 0}], 
 
>    {finiteWeight[4, {0, 0, 4, 0}], 1}, 6, 0}, 
 
>   {finiteWeight[4, {0, 0, 0, 0}], {finiteWeight[4, {0, 0, 0, 0}], 1}, 6, 
 
>    6 table$7784[finiteWeight[4, {0, 0, 0, 0}]]}}
5859
finiteWeight[4, {0, 0, 1, 1}]
{{finiteWeight[4, {0, 0, 3, 3}], {finiteWeight[4, {0, 0, 3, 3}], 1}, 1, 0}, 
 
>   {finiteWeight[4, {0, 0, 3, 0}], {finiteWeight[4, {0, 0, 3, 0}], 1}, -4, 
 
>    0}, {finiteWeight[4, {0, 0, 2, 1}], {finiteWeight[4, {0, 0, 2, 1}], 1}, 
 
>    -24, -4800}, {finiteWeight[4, {0, 0, 2, -1}], 
 
>    {finiteWeight[4, {0, 0, 2, 0}], -1}, -4, 23436}, 
 
>   {finiteWeight[4, {0, 0, 1, 0}], {finiteWeight[4, {0, 0, 1, 0}], 1}, -24, 
 
>    -24 table$7784[finiteWeight[4, {0, 0, 1, 0}]]}, 
 
>   {finiteWeight[4, {0, 0, 2, 2}], {finiteWeight[4, {0, 0, 2, 2}], 1}, 16, 
 
>    160}, {finiteWeight[4, {0, 0, 1, 2}], 
 
>    {finiteWeight[4, {0, 0, 1, 2}], 1}, -24, 
 
>    -24 table$7784[finiteWeight[4, {0, 0, 1, 2}]]}, 
 
>   {finiteWeight[4, {0, 0, 2, 0}], {finiteWeight[4, {0, 0, 2, 0}], 1}, 16, 
 
>    93744}, {finiteWeight[4, {0, 0, 0, 3}], 
 
>    {finiteWeight[4, {0, 0, 2, 1}], -1}, -4, 800}, 
 
>   {finiteWeight[4, {0, 0, -1, 2}], {finiteWeight[4, {0, 0, 1, 0}], -1}, -4, 
 
>    4 table$7784[finiteWeight[4, {0, 0, 1, 0}]]}, 
 
>   {finiteWeight[4, {0, 0, 3, 2}], {finiteWeight[4, {0, 0, 3, 2}], 1}, -4, 
 
>    0}, {finiteWeight[4, {0, 0, 2, 3}], {finiteWeight[4, {0, 0, 2, 3}], 1}, 
 
>    -4, -4 table$7784[finiteWeight[4, {0, 0, 2, 3}]]}, 
 
>   {finiteWeight[4, {0, 0, -1, 3}], {finiteWeight[4, {0, 0, 2, 0}], -1}, 1, 
 
>    -5859}, {finiteWeight[4, {0, 0, 0, 1}], 
 
>    {finiteWeight[4, {0, 0, 0, 1}], 1}, -24, 
 
>    -24 table$7784[finiteWeight[4, {0, 0, 0, 1}]]}, 
 
>   {finiteWeight[4, {0, 0, 1, 3}], {finiteWeight[4, {0, 0, 2, 2}], -1}, 6, 
 
>    -60}, {finiteWeight[4, {0, 0, 1, 1}], 
 
>    {finiteWeight[4, {0, 0, 1, 1}], 1}, 36, 
 
>    36 table$7784[finiteWeight[4, {0, 0, 1, 1}]]}, 
 
>   {finiteWeight[4, {0, 0, 0, 2}], {finiteWeight[4, {0, 0, 1, 1}], -1}, 16, 
 
>    -16 table$7784[finiteWeight[4, {0, 0, 1, 1}]]}, 
 
>   {finiteWeight[4, {0, 0, 3, -1}], {finiteWeight[4, {0, 0, 3, 0}], -1}, 1, 
 
>    0}, {finiteWeight[4, {0, 0, 0, 0}], {finiteWeight[4, {0, 0, 0, 0}], 1}, 
 
>    16, 16 table$7784[finiteWeight[4, {0, 0, 0, 0}]]}, 
 
>   {finiteWeight[4, {0, 0, 0, -1}], {finiteWeight[4, {0, 0, 0, 0}], -1}, -4, 
 
>    4 table$7784[finiteWeight[4, {0, 0, 0, 0}]]}, 
 
>   {finiteWeight[4, {0, 0, -1, 0}], {finiteWeight[4, {0, 0, -1, 0}], 1}, -4, 
 
>    -4 table$7784[finiteWeight[4, {0, 0, -1, 0}]]}, 
 
>   {finiteWeight[4, {0, 0, -1, -1}], {finiteWeight[4, {0, 0, -1, 0}], -1}, 
 
>    1, -table$7784[finiteWeight[4, {0, 0, -1, 0}]]}, 
 
>   {finiteWeight[4, {0, 0, 1, -1}], {finiteWeight[4, {0, 0, 1, 0}], -1}, 6, 
 
>    -6 table$7784[finiteWeight[4, {0, 0, 1, 0}]]}, 
 
>   {finiteWeight[4, {0, 0, 3, 1}], {finiteWeight[4, {0, 0, 3, 1}], 1}, 6, 
 
>    0}, {finiteWeight[4, {0, 0, -1, 1}], 
 
>    {finiteWeight[4, {0, 0, 0, 0}], -1}, 6, 
 
>    -6 table$7784[finiteWeight[4, {0, 0, 0, 0}]]}}
-107421
finiteWeight[4, {0, 0, 1, 0}]
{{finiteWeight[4, {0, 0, 3, 2}], {finiteWeight[4, {0, 0, 3, 2}], 1}, 1, 0}, 
 
>   {finiteWeight[4, {0, 0, 3, -1}], {finiteWeight[4, {0, 0, 3, 0}], -1}, -4, 
 
>    0}, {finiteWeight[4, {0, 0, 2, 0}], {finiteWeight[4, {0, 0, 2, 0}], 1}, 
 
>    -24, -140616}, {finiteWeight[4, {0, 0, 2, -2}], 
 
>    {finiteWeight[4, {0, 0, 2, 1}], -1}, -4, 800}, 
 
>   {finiteWeight[4, {0, 0, 1, -1}], {finiteWeight[4, {0, 0, 1, 0}], -1}, 
 
>    -24, 24 table$7784[finiteWeight[4, {0, 0, 1, 0}]]}, 
 
>   {finiteWeight[4, {0, 0, 2, 1}], {finiteWeight[4, {0, 0, 2, 1}], 1}, 16, 
 
>    3200}, {finiteWeight[4, {0, 0, 1, 1}], 
 
>    {finiteWeight[4, {0, 0, 1, 1}], 1}, -24, 2578104}, 
 
>   {finiteWeight[4, {0, 0, 2, -1}], {finiteWeight[4, {0, 0, 2, 0}], -1}, 16, 
 
>    -93744}, {finiteWeight[4, {0, 0, 0, 2}], 
 
>    {finiteWeight[4, {0, 0, 1, 1}], -1}, -4, -429684}, 
 
>   {finiteWeight[4, {0, 0, -1, 1}], {finiteWeight[4, {0, 0, 0, 0}], -1}, -4, 
 
>    4 table$7784[finiteWeight[4, {0, 0, 0, 0}]]}, 
 
>   {finiteWeight[4, {0, 0, 3, 1}], {finiteWeight[4, {0, 0, 3, 1}], 1}, -4, 
 
>    0}, {finiteWeight[4, {0, 0, 2, 2}], {finiteWeight[4, {0, 0, 2, 2}], 1}, 
 
>    -4, -40}, {finiteWeight[4, {0, 0, -1, 2}], 
 
>    {finiteWeight[4, {0, 0, 1, 0}], -1}, 1, 
 
>    -table$7784[finiteWeight[4, {0, 0, 1, 0}]]}, 
 
>   {finiteWeight[4, {0, 0, 0, 0}], {finiteWeight[4, {0, 0, 0, 0}], 1}, -24, 
 
>    -24 table$7784[finiteWeight[4, {0, 0, 0, 0}]]}, 
 
>   {finiteWeight[4, {0, 0, 1, 2}], {finiteWeight[4, {0, 0, 1, 2}], 1}, 6, 
 
>    6 table$7784[finiteWeight[4, {0, 0, 1, 2}]]}, 
 
>   {finiteWeight[4, {0, 0, 1, 0}], {finiteWeight[4, {0, 0, 1, 0}], 1}, 36, 
 
>    36 table$7784[finiteWeight[4, {0, 0, 1, 0}]]}, 
 
>   {finiteWeight[4, {0, 0, 0, 1}], {finiteWeight[4, {0, 0, 0, 1}], 1}, 16, 
 
>    16 table$7784[finiteWeight[4, {0, 0, 0, 1}]]}, 
 
>   {finiteWeight[4, {0, 0, 3, -2}], {finiteWeight[4, {0, 0, 3, 1}], -1}, 1, 
 
>    0}, {finiteWeight[4, {0, 0, 0, -1}], 
 
>    {finiteWeight[4, {0, 0, 0, 0}], -1}, 16, 
 
>    -16 table$7784[finiteWeight[4, {0, 0, 0, 0}]]}, 
 
>   {finiteWeight[4, {0, 0, 0, -2}], {finiteWeight[4, {0, 0, 0, 1}], -1}, -4, 
 
>    4 table$7784[finiteWeight[4, {0, 0, 0, 1}]]}, 
 
>   {finiteWeight[4, {0, 0, -1, -1}], {finiteWeight[4, {0, 0, -1, 0}], -1}, 
 
>    -4, 4 table$7784[finiteWeight[4, {0, 0, -1, 0}]]}, 
 
>   {finiteWeight[4, {0, 0, -1, -2}], {finiteWeight[4, {0, 0, 0, 0}], 1}, 1, 
 
>    table$7784[finiteWeight[4, {0, 0, 0, 0}]]}, 
 
>   {finiteWeight[4, {0, 0, 1, -2}], {finiteWeight[4, {0, 0, 1, 1}], -1}, 6, 
 
>    644526}, {finiteWeight[4, {0, 0, 3, 0}], 
 
>    {finiteWeight[4, {0, 0, 3, 0}], 1}, 6, 0}, 
 
>   {finiteWeight[4, {0, 0, -1, 0}], {finiteWeight[4, {0, 0, -1, 0}], 1}, 6, 
 
>    6 table$7784[finiteWeight[4, {0, 0, -1, 0}]]}}
-2562392
finiteWeight[4, {0, 0, 0, 0}]
{{finiteWeight[4, {0, 0, 2, 2}], {finiteWeight[4, {0, 0, 2, 2}], 1}, 1, 10}, 
 
>   {finiteWeight[4, {0, 0, 2, -1}], {finiteWeight[4, {0, 0, 2, 0}], -1}, -4, 
 
>    23436}, {finiteWeight[4, {0, 0, 1, 0}], 
 
>    {finiteWeight[4, {0, 0, 1, 0}], 1}, -24, 61497408}, 
 
>   {finiteWeight[4, {0, 0, 1, -2}], {finiteWeight[4, {0, 0, 1, 1}], -1}, -4, 
 
>    -429684}, {finiteWeight[4, {0, 0, 0, -1}], 
 
>    {finiteWeight[4, {0, 0, 0, 0}], -1}, -24, 
 
>    24 table$7784[finiteWeight[4, {0, 0, 0, 0}]]}, 
 
>   {finiteWeight[4, {0, 0, 1, 1}], {finiteWeight[4, {0, 0, 1, 1}], 1}, 16, 
 
>    -1718736}, {finiteWeight[4, {0, 0, 0, 1}], 
 
>    {finiteWeight[4, {0, 0, 0, 1}], 1}, -24, 
 
>    -24 table$7784[finiteWeight[4, {0, 0, 0, 1}]]}, 
 
>   {finiteWeight[4, {0, 0, 1, -1}], {finiteWeight[4, {0, 0, 1, 0}], -1}, 16, 
 
>    40998272}, {finiteWeight[4, {0, 0, -1, 2}], 
 
>    {finiteWeight[4, {0, 0, 1, 0}], -1}, -4, -10249568}, 
 
>   {finiteWeight[4, {0, 0, -2, 1}], {finiteWeight[4, {0, 0, 0, 0}], 1}, -4, 
 
>    -4 table$7784[finiteWeight[4, {0, 0, 0, 0}]]}, 
 
>   {finiteWeight[4, {0, 0, 2, 1}], {finiteWeight[4, {0, 0, 2, 1}], 1}, -4, 
 
>    -800}, {finiteWeight[4, {0, 0, 1, 2}], 
 
>    {finiteWeight[4, {0, 0, 1, 2}], 1}, -4, 
 
>    -4 table$7784[finiteWeight[4, {0, 0, 1, 2}]]}, 
 
>   {finiteWeight[4, {0, 0, -2, 2}], {finiteWeight[4, {0, 0, 1, 0}], 1}, 1, 
 
>    -2562392}, {finiteWeight[4, {0, 0, -1, 0}], 
 
>    {finiteWeight[4, {0, 0, -1, 0}], 1}, -24, 
 
>    -24 table$7784[finiteWeight[4, {0, 0, -1, 0}]]}, 
 
>   {finiteWeight[4, {0, 0, 0, 2}], {finiteWeight[4, {0, 0, 1, 1}], -1}, 6, 
 
>    644526}, {finiteWeight[4, {0, 0, 0, 0}], 
 
>    {finiteWeight[4, {0, 0, 0, 0}], 1}, 36, 
 
>    36 table$7784[finiteWeight[4, {0, 0, 0, 0}]]}, 
 
>   {finiteWeight[4, {0, 0, -1, 1}], {finiteWeight[4, {0, 0, 0, 0}], -1}, 16, 
 
>    -16 table$7784[finiteWeight[4, {0, 0, 0, 0}]]}, 
 
>   {finiteWeight[4, {0, 0, 2, -2}], {finiteWeight[4, {0, 0, 2, 1}], -1}, 1, 
 
>    -200}, {finiteWeight[4, {0, 0, -1, -1}], 
 
>    {finiteWeight[4, {0, 0, -1, 0}], -1}, 16, 
 
>    -16 table$7784[finiteWeight[4, {0, 0, -1, 0}]]}, 
 
>   {finiteWeight[4, {0, 0, -1, -2}], {finiteWeight[4, {0, 0, 0, 0}], 1}, -4, 
 
>    -4 table$7784[finiteWeight[4, {0, 0, 0, 0}]]}, 
 
>   {finiteWeight[4, {0, 0, -2, -1}], {finiteWeight[4, {0, 0, -1, 0}], -1}, 
 
>    -4, 4 table$7784[finiteWeight[4, {0, 0, -1, 0}]]}, 
 
>   {finiteWeight[4, {0, 0, -2, -2}], {finiteWeight[4, {0, 0, 0, 0}], -1}, 1, 
 
>    -table$7784[finiteWeight[4, {0, 0, 0, 0}]]}, 
 
>   {finiteWeight[4, {0, 0, 0, -2}], {finiteWeight[4, {0, 0, 0, 1}], -1}, 6, 
 
>    -6 table$7784[finiteWeight[4, {0, 0, 0, 1}]]}, 
 
>   {finiteWeight[4, {0, 0, 2, 0}], {finiteWeight[4, {0, 0, 2, 0}], 1}, 6, 
 
>    35154}, {finiteWeight[4, {0, 0, -2, 0}], 
 
>    {finiteWeight[4, {0, 0, -1, 0}], 1}, 6, 
 
>    6 table$7784[finiteWeight[4, {0, 0, -1, 0}]]}}
-88237426

Out[84]= formalElement[table$9509]


Out[26]= formalElement[table$1826]

Out[23]= formalElement[table$1565]

finiteWeight[4, {0, 0, -2, -2}]1

Out[10]= makeFormalElement[{finiteWeight[4, {0, 0, 1, 1}], 
 
>     finiteWeight[4, {0, 0, 0, 0}]}, {0, finiteWeight[4, {0, 0, 0, 0}]}]

Export["/home/anton/programing/Affine/tests/brc.png",Graphics[Text[br[#],{#[standardBase][[4]],#[standardBase][[3]]}] & /@ br[weights],Axes->True]]

Out[78]= /home/anton/programing/Affine/tests/brc.png

Out[75]= /home/anton/programing/Affine/tests/brc.png

Out[27]= /home/anton/programing/Affine/tests/brc.png


br0=simpleBranching[b4,b2][wg]

Export["/home/anton/programing/Affine/tests/brc0.png",Graphics[Text[br0[#],{#[standardBase][[4]],#[standardBase][[3]]}] & /@ br0[weights]]]

Out[54]= /home/anton/programing/Affine/tests/brc.png


ort=orthogonalSubsystem[b4,b2]

dimension[ort][makeFiniteWeight[{1,-1,0,0}]]

dimension[makeSimpleRootSystem[B,2]][makeFiniteWeight[{1,-1}]]


dimension[{makeFiniteWeight[{1,1}]}][makeFiniteWeight[{2,2}]]

b2

