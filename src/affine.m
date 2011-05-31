BeginPackage["Affine`"]

Expect::"usage"="Expect[comment,value,expression] is used for unit tests. If expression != value it throws exception.";

standardBase::"usage"=
    "standardBase[coordinates] represents weight vector in standard (Bourbaki) coordinates.\
    E.g. simple roots for B2 are standardBase[1,-1] and standardBase[0,1]";

finiteWeight::"usage"=
    "finiteWeight[dimension_?NumberQ,coordinates_standardBase] represents\ 
    vector in weight space of finite-dimensional Lie algebra.\n 
    finiteWeight[dimension] returns dimension of the space, where weight vector is embedded\ 
    (i.e. for sl_n it is n+1).\n 
    finiteWeight[standardBase] returns standard base coordinates of weight of finite-dimensional Lie algebra";

dimension::"usage"=
    "dimension[wg_?weightQ] returns the dimension of the space, where finite part of weight lives";

finitePart::"usage"=
    "finitePart[wg_?weightQ] returns finite part of weight";

standardBase::"usage"=
    "standardBase[wg_?weightQ] returns coorinates of finite part of finite part of weight in standard (Bourbaki) basis";

makeFiniteWeight::"usage"=
    "makeFiniteWeight[{coordinates__?NumberQ}] creates finiteWeight with given coordinates in standard base";

Dot::"usage"=Dot::"usage" <> "\n It is defined for weights of finite and affine Lie algebras";

Plus::"usage"=Plus::"usage" <> "\n It is defined for weights of finite and affine Lie algebras";

Equal::"usage"=Equal::"usage" <> "\n It is defined for weights of finite and affine Lie algebras";

Times::"usage"=Times::"usage"<>"\n\n Mutliplication by numbers is defined for the elements of weight space of affine and finite-dimensional Lie algebras.\ 
    \n For example makeFiniteWeight[{1,2,3}]*2==makeFiniteWeight[{2,4,6}]]";

affineWeight::"usage"=
    "affineWeight[dimension_?NumberQ,fw_finiteWeigt,level_?NumberQ, grade_?NumberQ] represents\ 
    vector in weight space of affine Lie algebra.\n 
    affineWeight[dimension] returns dimension of the space of real roots, where finite weight vector is embedded\ 
    (i.e. for sl_n it is n+1).\n 
    affineWeight[finitePart] returns finite part of weight as finiteWeight structure\n
    affineWeight[level] returns level of affine weight\n
    affineWeight[grade] returns grade of affine weight";

level::"usage"=
    "level[w_affineWeight] or w[level] returns level of weight of affine Lie algebra";

grade::"usage"=
    "grade[w_affineWeight] or w[grade] returns grade of weight of affine Lie algebra";

makeAffineWeight::"usage"= 
    "makeAffineWeight[fw_finiteWeight,level_?NumberQ,grade_?NumberQ] creates affine weight with the given finite part fw, level and grade\n
    makeAffineWeight[{fw__?NumberQ},level_?NumberQ,grade_?NumberQ] creates affine weight with the given finite part fw, level and grade
    ";

makeHashtable::"usage"=
    "makeHashtable[keys_List,values_List] creates hashtable from the list keys and list of values";
keys::"usage"="keys[hashtable] gives all the keys in hashtable";
values::"usage"="values[hashtable] gives all the values in hashtable (in the same order as keys)";

finiteRootSystem::"usage"=
    "finiteRootSystem[rank_Integer,{roots_finiteWeight}] represents root system of finite-dimensional Lie algebra.\n
    finiteRootSystem[rank] returns rank of the root system\n
    finiteRootSystem[dimension] returns the dimension of the space where the roors are realized as the vectors\n
    finiteRootSystem[simpleRoots] returns unsorted list of simple roots in the root system.\n
    finiteRootSystem[simpleRoot][n_Integer] returns n'th simple root"

makeFiniteRootSystem::"usage"=
    "makeFiniteRootSystem[{roots__finiteWeight}] creates finiteRootSystem structure from the List of simple roots\n
    makeFiniteRootSystem[{roots__finiteWeight}] creates  finiteRootSystem structure from the List of simple roots\n
    given as the lists of coordinates";

rank::"usage"=
    "rank[rs_?rootSystemQ] or rs[rank] returns rank of root system. Rank of affine root system is equal to rank of underlying \n
    finite-dimensional root system";

simpleRoot::"usage"=
    "simpleRoot[rs_?rootSystemQ][i_Integer] or rs[simpleRoot][i_Integer] returns simple root with the index i. For affine root system i can be zero";

simpleRoots::"usage"=
    "simpleRoots[rs_?rootSystemQ] or rs[simpleRoots] returns unsorted list of simple roots";

prependZeros::"usage"=
    "prependZeros[num_Integer,vec_finiteWeight] embeds vec to bigger space by prepending num zeros to its coordinates";

appendZeros::"usage"=
    "appendZeros[num_Integer,vec_finiteWeight] embeds vec to bigger space by appending num zeros to its coordinates";

CirclePlus::"usage"=CirclePlus::"usage" <> "\n Direct sum of finite-dimensional and affine Lie algebras can be specified as sum of root systems";

makeSimpleRootSystem::"usage"=
    "makeSimpleRootSystem[series, rank] creates root system of the algebra from given series with given rank.\n
    Exceptional Lie algebras are not supported yet";

Subscript::"usage"=Subscript::"usage" <>
    "\n Subscript[A|B|C|D|E,n_Integer] creates root system of the algebra from given series with given rank. \n
    It is an equivalent to makeSimpleRootsSystem, but looks better in notebook interface."

A::"usage"="represents root systems of series A";
B::"usage"="represents root systems of series B";
C::"usage"="represents root systems of series C";
D::"usage"="represents root systems of series D";
E::"usage"="represents root systems of series E";

weightQ::"usage"=
    "Weight predicate"

Begin["`Private`"]

Expect[ description_, val_, expr_ ] := 
    If[
	val != expr,
	Throw[
	    StringJoin[ ToString[description],": GOT UNEXPECTED VALUE ", ToString[expr], 
			" INSTEAD OF ", ToString[val] ]
	    , "assertion exception"
	     ]
      ]


finiteWeight/:x_finiteWeight[dimension]:=x[[1]];
finiteWeight/:x_finiteWeight[standardBase]:=x[[2]];
finiteWeight/:x_finiteWeight[finitePart]:=x;

finitePart[wg_?weightQ]=wg[finitePart];
dimension[wg_?weightQ]=wg[dimension];
standardBase[wg_?weightQ]=wg[finitePart][standardBase];

(*makeFiniteWeight[{coordinates__?NumberQ}]:=finiteWeight @@ {Length[{coordinates}],{coordinates}}*)
makeFiniteWeight[{coordinates__}]:=finiteWeight @@ {Length[{coordinates}],{coordinates}}

finiteWeight/:x_finiteWeight . y_finiteWeight:=x[standardBase].y[standardBase]

finiteWeight/:Plus[wgs__finiteWeight]:=
    makeFiniteWeight[Plus @@ (#[standardBase]&/@ {wgs})]
    
finiteWeight/:x_finiteWeight==y_finiteWeight:=x[standardBase]==y[standardBase]

finiteWeight/:0*y_finiteWeight:=makeFiniteWeight[0*y[standardBase]];

finiteWeight/:x_?NumberQ*y_finiteWeight:=makeFiniteWeight[x*y[standardBase]];

affineWeight/:x_affineWeight[dimension]:=x[[1]];
affineWeight/:x_affineWeight[finitePart]:=x[[2]];
affineWeight/:x_affineWeight[level]:=x[[3]];
affineWeight/:x_affineWeight[grade]:=x[[4]];

level[x_affineWeight]=x[level];

grade[x_affineWeight]:=x[grade];
grade[x_finiteWeight]:=0;

makeAffineWeight[fp_finiteWeight,lev_?NumberQ,gr_?NumberQ]:=affineWeight[fp[dimension],fp,lev,gr];
makeAffineWeight[{fp__?NumberQ},lev_?NumberQ,gr_?NumberQ]:=makeAffineWeight[makeFiniteWeight[{fp}],lev,gr]

affineWeight/:x_affineWeight==y_affineWeight:=And[x[finitePart]==y[finitePart],
						  x[level]==y[level],
						  x[grade]==y[grade]]

affineWeight/:x_affineWeight+y_affineWeight:=
    makeAffineWeight[x[finitePart]+y[finitePart],
		     x[level]+y[level],
		     x[grade]+y[grade]]

affineWeight/:x_affineWeight.y_affineWeight:= 
    x[finitePart].y[finitePart] + 
    x[level]* y[grade] + 
    x[grade]* y[level]

affineWeight/:x_?NumberQ*y_affineWeight:=makeAffineWeight[x*y[finitePart],x*y[level],x*y[grade]]

ExpandNCM[(h : NonCommutativeMultiply)[a___, b_Plus, c___]] :=  Distribute[h[a, b, c], Plus, h, Plus, ExpandNCM[h[##]] &];
ExpandNCM[a_] := ExpandAll[a];
ExpandNCM[(a + b) ** (a + b) ** (a + b)];

keys = DownValues[#,Sort->False][[All,1,1,1]]&;

values = Function[ht,(ht[#]&)/@keys[ht]]

hasKey[hashtable_,key_]:=hashtable[key]=!=Unevaluated[hashtable[key]]

makeHashtable[keys_List,values_List]/; Length[keys]==Length[values] :=
    Module[{table},
	   DownValues[table]=((table[#[[1]]] -> #[[2]] &)/@ Transpose[{keys,values}]);
	   table]

makeFiniteRootSystem[{roots__finiteWeight}]:=finiteRootSystem[Length[{roots}],{roots}[[1]][dimension],{roots}]

makeFiniteRootSystem[{roots__List}]:=makeFiniteRootSystem[makeFiniteWeight/@{roots}]

finiteRootSystem/:x_finiteRootSystem[rank]:=x[[1]];
finiteRootSystem/:x_finiteRootSystem[dimension]:=x[[2]];
finiteRootSystem/:x_finiteRootSystem[simpleRoots]:=x[[3]];
finiteRootSystem/:x_finiteRootSystem[simpleRoot][0]:=highestRoot[x];
finiteRootSystem/:x_finiteRootSystem[simpleRoot][n_Integer]:=x[[3]][[n]];

rank[rs_?rootSystemQ]=rs[rank];
simpleRoot[rs_?rootSystemQ]=rs[simpleRoot];
simpleRoots[rs_?rootSystemQ]=rs[simpleRoots];
dimension[rs_?rootSystemQ]=rs[dimension];

prependZeros[num_Integer,vec_finiteWeight]:=makeFiniteWeight[Join[Table[0,{num}],vec[standardBase]]];

appendZeros[num_Integer,vec_finiteWeight]:=makeFiniteWeight[Join[vec[standardBase],Table[0,{num}]]];

finiteRootSystem/:CirclePlus[x_finiteRootSystem,y_finiteRootSystem]:=makeFiniteRootSystem[Join[Map[appendZeros[y[dimension],#]&,x[simpleRoots]],
											       Map[prependZeros[x[dimension],#]&,y[simpleRoots]]]];

affineRootSystem/:CirclePlus[x_affineRootSystem,y_affineRootSystem]:=makeAffineExtension[CirclePlus[x[finiteRootSystem],y[finiteRootSystem]]];

makeSimpleRootSystem[A,1]:=makeFiniteRootSystem[{{1}}];
makeSimpleRootSystem[A,r_Integer]:=makeFiniteRootSystem[makeFiniteWeight /@ Table[If[i==j,1,If[i==j-1,-1,0]],{i,1,r},{j,1,r+1}]];
makeSimpleRootSystem[B,rank_Integer]:=makeFiniteRootSystem[Append[Table[If[i==j,1,If[i==j-1,-1,0]],{i,1,rank-1},{j,1,rank}],Append[Table[0,{rank-1}],1]]];
makeSimpleRootSystem[C,rank_Integer]:=makeFiniteRootSystem[Append[Table[If[i==j,1,If[i==j-1,-1,0]],{i,1,rank-1},{j,1,rank}],Append[Table[0,{rank-1}],2]]];
makeSimpleRootSystem[D,rank_Integer]:=makeFiniteRootSystem[Append[Table[If[i==j,1,If[i==j-1,-1,0]],{i,1,rank-1},{j,1,rank}],Append[Append[Table[0,{rank-2}],1],1]]];

Subscript[A,n_Integer]:=makeSimpleRootSystem[A,n];
Subscript[B,n_Integer]:=makeSimpleRootSystem[B,n];
Subscript[C,n_Integer]:=makeSimpleRootSystem[C,n];
Subscript[D,n_Integer]:=makeSimpleRootSystem[D,n];


checkGrade::"usage"=
    "computations for affine Lie algebras are limited by some grade, by default it is equal to 10. To change it set rs[gradeLimit].\n
    checkGrade[rs_?rootSystemQ][w_?weightQ] checks if given weight has absolute value of grade less then gradeLimit. Returns true 
    for finite-dimensional case";
checkGrade[rs_][w_finiteWeight]=True;
checkGrade[rs_][w_affineWeight]:=(Abs[w[grade]]<rs[gradeLimit]) /. rs[gradeLimit]->10;
checkGrade[gr_NumberQ][w_affineWeight]:=Abs[w[grade]]<=gr;

Expect["checkGrade finite dimensional Lie algebra", True, checkGrade[makeSimpleRootSystem[B,2]][makeFiniteWeight[{2,1}]]]

Expect["checkGrade affine Lie algebra", False, checkGrade[makeAffineExtension[makeSimpleRootSystem[A,2]]][makeAffineWeight[{2,1,1},1,10]]]

Module[{b2a=makeAffineExtension[makeSimpleRootSystem[B,2]]},
       b2a[gradeLimit]=15;
       Expect["checkGrade affine Lie algebra", True, checkGrade[b2a][makeAffineWeight[{2,1},1,10]]];
       b2a[gradeLimit]=.;]

weightQ[x_finiteWeight]=True;
weightQ[x_affineWeight]=True;
weightQ[_]=False;

Expect["weight predicate on finite weights", True, weightQ[makeFiniteWeight[{1,2,3}]]]

Expect["weight predicate on finite weights", False, weightQ[makeFiniteWeight[1,2,3]]]

Expect["weight predicate on affine weights", True, weightQ[makeAffineWeight[{1,2,3},1,1]]]

Expect["weight predicate on affine weights", False, weightQ[makeAffineWeight[1,2,3]]]

reflection::"usage"=
    "reflection[x_finiteWeight] represents reflection in the hyperplane, orthogonal to the given root x\n
    reflection[x_affineWeight] represents affine reflection in the hyperplane, orthogonal to the given root x\n
    reflection[x][y] reflects y in the hyperplane, orthogonal to x"

reflection[x_?weightQ]:=Function[y, y-2*(x.y)/(x.x)*x];

Expect["Reflection for finite weights", True,reflection[makeFiniteWeight[{1,0}]][makeFiniteWeight[{1,1}]]==makeFiniteWeight[{-1,1}]]

coroot::"usage"="returns coroot of given root"
    
coroot[x_?weightQ]:=2*x/(x.x);

Expect["Co root of [1,0]", True, coroot[makeFiniteWeight[{1,0}]]==makeFiniteWeight[{2,0}]]

Expect["Co root of affine [1,0]", True, coroot[makeAffineWeight[{1,0},1,0]]==makeAffineWeight[{2,0},2,0]]

cartanMatrix::"usage"=
    "cartanMatrix[rs_?rootSystemQ] returns Cartan matrix of root system rs";
cartanMatrix[rs_?rootSystemQ]:=Transpose[Outer[Dot,rs[simpleRoots],coroot/@rs[simpleRoots]]];

Expect["Cartan matrix of B2",{{2, -1}, {-2, 2}},cartanMatrix[makeSimpleRootSystem[B,2]]]

revApply[x_,f_]:=f[x];

(* Print[makeSimpleRootSystem[B,2][simpleRoot][2]] *)

rootSystemQ::"usage"=
    "Predicate for root system data structures";
rootSystemQ[rs_]:=MatchQ[rs,x_finiteRootSystem|x_affineRootSystem]

Expect["Predicate for finite and affine root systems",True,rootSystemQ[makeSimpleRootSystem[B,2]]]

weylGroupElement::"usage"=
    "weylGroupElement[x__Integer][rs_?rootSystemQ] represents element of Weyl group composed of basic reflecetions x \n
    of root system rs. Example: weylGroupElement[1,2,1][makeSimpleRootSystem[B,2]] = s1 s2 s1";
clear[weylGroupElement];
weylGroupElement[rs_?rootSystemQ][x__Integer]:=Function[z,Fold[revApply,z,reflection /@ rs[simpleRoot]/@{x}]];

Expect["Weyl reflection s1 s2 s1 in algebra B2",True,
       weylGroupElement[makeSimpleRootSystem[B,2]][1,2,1][makeFiniteWeight[{1,0}]]==
       makeFiniteWeight[{-1,0}]]

fundamentalWeights::"usage"=
    "fundamentalWeights[rs_?rootSystemQ] calculates fundamental weights of given root system rs";
fundamentalWeights[rs_finiteRootSystem]:=Plus@@(Inverse[cartanMatrix[rs]]*rs[simpleRoots]);

Expect["Fundamental weights for B2", True, {makeFiniteWeight[{1,0}],makeFiniteWeight[{1/2,1/2}]}==fundamentalWeights[makeSimpleRootSystem[B,2]]]

Expect["Fundamental weights for A1", True, {makeFiniteWeight[{1/2}]}==fundamentalWeights[makeSimpleRootSystem[A,1]]]

Expect["Fundamental weights for A2", True, {makeFiniteWeight[{2/3,-1/3,-1/3}],makeFiniteWeight[{1/3,1/3,-2/3}]}==fundamentalWeights[makeSimpleRootSystem[A,2]]]

rho::"usage"=
    "rho[rs_?rootSystemQ] - Weyl vector of root system rs (sum of fundamental weights)";
rho[rs_?rootSystemQ]:=Plus@@fundamentalWeights[rs];

rho[{posroots__finiteWeight}]:=1/2*(Plus@@{posroots});

Expect["Weyl vector for B2",True,makeFiniteWeight[{3/2,1/2}]==rho[makeSimpleRootSystem[B,2]]]

Expect["Weyl vector for B2",True,makeFiniteWeight[{3/2,1/2}]==rho[positiveRoots[makeSimpleRootSystem[B,2]]]]

toFundamentalChamber::"usage"=
    "toFundamentalChamber[rs_?rootSystemQ][vec_?weightQ] acts on a weight vec by simple reflections of rs till it gets to main Weyl chamber";
toFundamentalChamber[rs_?rootSystemQ][vec_?weightQ]:=
    First[NestWhile[Function[v,
			     reflection[Scan[If[#.v<0,Return[#]]&,rs[simpleRoots]]][v]],
		    vec,
		    Head[#]=!=reflection[Null]&]]

toFundamentalChamberWithParity[rs_?rootSystemQ][vec_?weightQ]:=
    ({#[[1]][[1]],-#[[2]]})& @ NestWhile[Function[v,
						  {reflection[Scan[If[#.v[[1]]<0,Return[#]]&,rs[simpleRoots]]][v[[1]]],-v[[2]]}],
					 {vec,1},
					 Head[#[[1]]]=!=reflection[Null]&]

Expect["To fundamental chamber",True,makeFiniteWeight[{1,1/2}]==toFundamentalChamber[makeSimpleRootSystem[B,2]][makeFiniteWeight[{-1,1/2}]]]

Expect["We can use this and other functions for mapping",True,
       Map[toFundamentalChamber[makeSimpleRootSystem[B,2]],{makeFiniteWeight[{-1,-1}],makeFiniteWeight[{-2,-1}]}]==
       {makeFiniteWeight[{1, 1}], makeFiniteWeight[{2, 1}]}]


mainChamberQ::"usage"=
    "mainChamberQ[rs_?rootSystemQ][wg_?weightQ] tells if weights wg lies inside the main Weyl chamber of root system rs or not";
mainChamberQ[{roots__?weightQ}][wg_?weightQ]:=And@@(#.wg>=0&/@{roots});
mainChamberQ[rs_?rootSystemQ][wg_?weightQ]:=mainChamberQ[rs[simpleRoots]][wg];

Expect["Main chamber predicate",True,mainChamberQ[makeSimpleRootSystem[B,2]][toFundamentalChamber[makeSimpleRootSystem[B,2]][makeFiniteWeight[{-1,1/2}]]]]



partialOrbit::"usage"=
    "partialOrbit[rs_finiteRootSystem][{weights__finiteWeight}] constructs Weyl partial orbit of given set of weights. \n
    It starts with the list of weights and reflects them away from main Weyl chamber.\n
    For w in fundamental chamber it gives the whole orbit.";
partialOrbit[rs_?rootSystemQ][{weights___?weightQ}]:=
    Most[NestWhileList[
	Function[x,
		 Union[
		     Flatten[
			 Map[
			     Function[y,
				      Map[reflection[#][y]&,
					  Cases[rs[simpleRoots],
						z_ /; And[z.y>0,
							  checkGrade[rs][reflection[z][y]]]
					       ]
					 ]
				     ],
			     x]
			    ],
		     SameTest->(#1==#2&)]
		],
	{weights},
	#=!={}&]];

Module[{b2=makeSimpleRootSystem[B,2]},
       Expect["Partial orbit test", True, partialOrbit[b2][{rho[b2]}]==
	      {{makeFiniteWeight[{3/2,1/2}]},{makeFiniteWeight[{1/2,3/2}],makeFiniteWeight[{3/2,-1/2}]},
	       {makeFiniteWeight[{-1/2,3/2}],makeFiniteWeight[{1/2,-3/2}]},
	       {makeFiniteWeight[{-3/2,1/2}],makeFiniteWeight[{-1/2,-3/2}]},
	       {makeFiniteWeight[{-3/2,-1/2}]}}]]

orbit::"usage"=
    "orbit[rs_?rootSystemQ][wg_?weightQ] returns the weight of Weyl orbit, containing given weight wg. \n
    Orbit is given as the list of lists starting with the weight in dominant Weyl chamber\n
    orbit[rs_?rootSystemQ][{wg_?weightQ}] works for a list of weights";

orbit[rs_?rootSystemQ][weight_?weightQ]:=partialOrbit[rs][{toFundamentalChamber[rs][weight]}];
orbit[rs_?rootSystemQ][{weights___?weightQ}]:=partialOrbit[rs][toFundamentalChamber[rs] /@ {weights}];

Module[{b2=makeSimpleRootSystem[B,2]},
       Expect["orbit is equivalent to partial orbit",True, partialOrbit[b2][{rho[b2]}]== orbit[b2][rho[b2]]]]


positiveRoots::"usage"=
    "positiveRoots[rs_?rootSystemQ] returns positive roots of root system rs";
positiveRoots[rs_finiteRootSystem]:=Map[-#&,Flatten[partialOrbit[rs][Map[-#&,rs[simpleRoots]]]]]


Module[{b2=makeSimpleRootSystem[B,2]},
       Expect["Positive roots of B2",True, positiveRoots[b2]=={makeFiniteWeight[{1,-1}],makeFiniteWeight[{0,1}],
							       makeFiniteWeight[{1,0}],makeFiniteWeight[{1,1}]}]]

(*orbit[makeSimpleRootSystem[B,2]][rho[makeSimpleRootSystem[B,2]]]

  Print/@positiveRoots[makeSimpleRootSystem[B,2]]

  Map[Print,orbit[makeSimpleRootSystem[B, 2]][rho[makeSimpleRootSystem[B, 2]]],2]

  SubValues[finiteWeight]
  *)


dimension::"usage"=
    "dimension[rs_?rootSystemQ][hweight_?weightQ] returns dimension of Lie algebra highest weight representation";
(*dimension[{pr__?weightQ}][hweight_?weightQ]:=*)
dimension[{pr__}][hweight_]:=
    Module[{rh=rho[{pr}]},
	   Times@@((  (#.(hweight+rh))/(rh.#) )&/@{pr})];

Expect["Dimension",5, dimension[{makeFiniteWeight[{1,1}]}][makeFiniteWeight[{2,2}]]]

dimension[rs_?rootSystemQ][hweight_?weightQ]:=dimension[positiveRoots[rs]][toFundamentalChamber[rs][hweight]];

Expect["Dimension",5, dimension[makeSimpleRootSystem[A,1]][makeFiniteWeight[{2}]]]

weightSystem::"usage"=
    "weightSystem[rs_?rootSystemQ][highestWeight_?weightQ] returns the set of dominant weights in the highest weight module. \n
    The list is split in pieces by number of root substractions";

weightSystem[{posroots__?weightQ}][highestWeight_?weightQ]:=Module[{minusPosRoots=-{posroots},mgrade=Max[grade/@{posroots}]},
								   (*								  Throw["_TODO_","not implemented"];*)
								   Most[NestWhileList[Function[x,Complement[
								       Cases[Flatten[Outer[Plus,minusPosRoots,x]],y_/;
									     And[checkGrade[mgrade][y],mainChamberQ[{posroots}][y]]]
								       ,x]],{highestWeight},#=!={}&]]];

weightSystem[rs_?rootSystemQ][highestWeight_?weightQ]:=Module[{minusPosRoots=-positiveRoots[rs]},
							      Most[NestWhileList[Function[x,Complement[
								  Cases[Flatten[Outer[Plus,minusPosRoots,x]],y_/;
									And[checkGrade[rs][y],mainChamberQ[rs][y]]]
								  ,x]],{highestWeight},#=!={}&]]];

Module[{b2=makeSimpleRootSystem[B,2]},
       Expect["Weights of [2,1] module of B2",True, weightSystem[b2][makeFiniteWeight[{2,1}]]==
	      {{makeFiniteWeight[{2,1}]},
	       {makeFiniteWeight[{1,0}],makeFiniteWeight[{1,1}],makeFiniteWeight[{2,0}]},
	       {makeFiniteWeight[{0,0}]}}]]

freudenthalMultiplicities::"usage"=
    "freudenthalMultiplicities[rs_finiteRootSystem][highestWeight_finiteWeight] returns hashtable with the multiplicities of 
    weights in the highest weight module";
freudenthalMultiplicities[rs_?rootSystemQ][highestWeight_?weightQ]:=
    Module[{rh=rho[rs],weights,mults,c,insideQ,
	    posroots=positiveRoots[rs],
	    toFC=toFundamentalChamber[rs]},
	   weights=SortBy[ Rest[Flatten[weightSystem[rs][highestWeight]]], -#.rh&];
	   c:=(#+rh).(#+rh)&;
	   mults[highestWeight]=1;
	   insideQ:=And[checkGrade[rs][#],IntegerQ[mults[toFC[#]]]]&;
	   Scan[Function[v,
			 mults[v]=
			 2/(c[highestWeight]-c[v])*
			 Plus@@
			 Map[Function[r,
				      Plus@@Map[mults[toFC[#[[1]]]]*#[[2]]&,
						Rest[NestWhileList[({#[[1]]+r,#[[2]]+r.r})&,
								   {v,v.r},
								   insideQ[#[[1]]+r]&]]]]
			     ,posroots]],
		weights];
	   mults];

Module[{b2=makeSimpleRootSystem[B,2],fm},
       fm=freudenthalMultiplicities[b2][makeFiniteWeight[{2,1}]];
       Expect["Mutliplicites of [2,1] B2 representation",{1, 1, 2, 3, 3},
	      fm[#]&/@keys[fm]]]

(* freudenthalMultiplicities[makeSimpleRootSystem[B,2]][makeFiniteWeight[{5,5}]] *)
orbitWithEps::"usage"="returns orbit with the determinants of Weyl reflections";
orbitWithEps[rs_?rootSystemQ][weight_?weightQ]:=Flatten[MapIndexed[Function[{x,i},Map[{#,(-1)^(i[[1]]+1)}&,x]],orbit[rs][weight]],1];

(* Expect["orbitWithEps __TODO__",False,True] *)

racahMultiplicities::"usage"=
    "racahMultiplicities[rs_?rootSystemQ] returns hashtable with the multiplicities of 
    weights in the highest weight module. It uses recurrent relation similar to Racah formula";

racahMultiplicities[rs_?rootSystemQ][highestWeight_?weightQ]:=
    Module[{rh=rho[rs],weights,mults,c,insideQ,
	    fan,
	    toFC=toFundamentalChamber[rs]},
	   fan=Map[{rh-#[[1]],#[[2]]}&,Rest[orbitWithEps[rs][rh]]];
	   weights=Sort[ Rest[Flatten[weightSystem[rs][highestWeight]]], #1.rh>#2.rh&];
	   mults[highestWeight]=1;
	   insideQ:=IntegerQ[mults[toFC[#]]]&;
	   Scan[Function[v,
			 mults[v]=
			 Plus@@(fan /. {x_?weightQ,e_Integer}:> If[insideQ[v+x],-e*mults[toFC[v+x]],0])],
		weights];
	   mults]

Module[{b2=makeSimpleRootSystem[B,2],fm,rm},
       fm=freudenthalMultiplicities[b2][makeFiniteWeight[{2,1}]];
       rm=freudenthalMultiplicities[b2][makeFiniteWeight[{2,1}]];
       Expect["Racah and Freudenthal formulae should give the same result",rm[#]&/@keys[rm],
	      fm[#]&/@keys[fm]]]

(* racahMultiplicities[makeSimpleRootSystem[B,2]][makeFiniteWeight[{5,5}]] *)

(* b2=makeSimpleRootSystem[B,2];
   rh=rho[b2];
   rs=b2; 

   fan=Map[{rh-#[[1]],#[[2]]}&,Rest[orbitWithEps[rs][rh]]]; *)

highestRoot::"usage"="returns highest root of root system";
highestRoot[rs_finiteRootSystem]:=toFundamentalChamber[rs][rs[simpleRoot][Ordering[(#.#&)/@rs[simpleRoots],-1][[1]]]]

Expect["Highest root for B2",makeFiniteWeight[{1, 1}],highestRoot[makeSimpleRootSystem[B,2]]]

makeAffineExtension::"usage"=
    "makeAffineExtension[fs_finiteRootSystem] creates root system of affine Lie algebra which 
    is extension of given finite-dimensional root system";
makeAffineExtension[fs_finiteRootSystem]:=affineRootSystem[fs[rank],fs,makeAffineWeight[-highestRoot[fs],0,1],(makeAffineWeight[#,0,0]&)/@fs[simpleRoots]]

OverHat::"usage"=
    "OverHat[rs_finiteRootSystem] creates affine extension of root system rs, it is equivalent makeAffineExtension[rs], but uses mathematical notation in notebook interface";
OverHat[rs_finiteRootSystem]:=makeAffineExtension[rs];


affineRootSystem::"usage"=
    "affineRootSystem[rank_Integer,finiteRootSystem_finiteRootSystem,imaginaryRoot_affineWeight,realRoots_List] 
    represents root system of affine Lie algebra.\n
    affineRootSystem[rank] returns rank of the root system\n
    affineRootSystem[imaginaryRoot] returns imaginary (zeroth) root\n
    affineRootSystem[realRoots] returns list of real roots (roots of finite-dimensional subalgebra)
    affineRootSystem[dimension] returns the dimension of the space where the roots are realized as the vectors\n
    affineRootSystem[simpleRoots] returns unsorted list of simple roots in the root system.\n
    affineRootSystem[simpleRoot][n_Integer] returns n'th simple root"


affineRootSystem/:rs_affineRootSystem[rank]:=rs[[1]]

affineRootSystem/:rs_affineRootSystem[finiteRootSystem]:=rs[[2]]

affineRootSystem/:rs_affineRootSystem[realRoots]:=rs[[4]]

affineRootSystem/:rs_affineRootSystem[imaginaryRoot]:=rs[[3]]

affineRootSystem/:rs_affineRootSystem[simpleRoots]:=Prepend[rs[[4]],rs[[3]]]

affineRootSystem/:rs_affineRootSystem[simpleRoot][0]:=rs[[3]];
affineRootSystem/:rs_affineRootSystem[simpleRoot][n_?NumberQ]/;n<=rs[rank]:=rs[simpleRoots][[n+1]];

Expect["simpleRoots of affine Lie algebra A1",True, OverHat[Subscript[A,1]][simpleRoot][1]==makeAffineWeight[{1},0,0]]

zeroWeight::"usage"="zeroWeight[rs_?rootSystemQ] returns zero root of root system rs (zero of rs[dimension]-dimemsional space)"; 
zeroWeight[rs_finiteRootSystem]:=makeFiniteWeight[Table[0,{rs[dimension]}]];
zeroWeight[rs_affineRootSystem]:=makeAffineWeight[zeroWeight[rs[finiteRootSystem]],0,0];

(* Ugly hack *)
positiveRoots[rs_affineRootSystem]:=Join[Map[-#&,Flatten[partialOrbit[rs][Map[-#&,rs[simpleRoots]]]]],
					 Join@@Table[NestWhileList[#+makeAffineWeight[zeroWeight[rs[finiteRootSystem]],0,1]&,zeroWeight[rs],checkGrade[rs][#]&],{rs[rank]}]];

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


toFundamentalChamber[rs_affineRootSystem][vec_affineWeight]:=
    First[NestWhile[Function[v,
			     reflection[Scan[If[#.v<0,Return[#]]&,rs[simpleRoots]]][v]],
		    vec,
		    Head[#]=!=reflection[Null]&]]

Expect["To main Weyl chamber for affine B2", True,Module[{b2a=OverHat[Subscript[B,2]]}, toFundamentalChamber[b2a][weight[b2a][1,-1,1]]==weight[b2a][0,0,1]]]

marks::"usage"="marks[rs_affineRootSystem] returns marks of affine Lie algebra";
marks[rs_affineRootSystem]:=Prepend[Inverse[cartanMatrix[rs[finiteRootSystem]]].(-2*#.rs[simpleRoot][0]/(#.#)&)/@rs[realRoots],1]

Expect["Marks for affine A_3", {1,1,1,1},marks[OverHat[Subscript[A,3]]]]

Expect["Marks for affine C_4", {1, 2, 2, 2, 1},marks[OverHat[Subscript[C,4]]]]

marks::"usage"="comarks[rs_affineRootSystem] returns comarks of affine Lie algebra";
comarks[rs_affineRootSystem]:=marks[rs]*Map[#.#/2&,rs[simpleRoots]]

Expect["Comarks for affine D_4", {1, 1, 2, 1, 1}, comarks[OverHat[Subscript[D,4]]]]

fundamentalWeights[rs_affineRootSystem]:=Map[makeAffineWeight[#[[1]],#[[2]],0]&,
					     Transpose[{Prepend[fundamentalWeights[rs[finiteRootSystem]],
								zeroWeight[rs[finiteRootSystem]]],
							comarks[rs]}]]

weight::"usage"=
    "weight[rs_?rootSystemQ][labels__Integer] constructs weight defined by Dynkin labels";
weight[rs_?rootSystemQ][labels__Integer]:=fundamentalWeights[rs].{labels}

Expect["weight of B_2", True,makeFiniteWeight[{3,1}]==weight[makeSimpleRootSystem[B,2]][2,2]]

dynkinLabels::"usage"=
    "dynkinLabels[rs_?rootSystemQ][wg_?weightQ] returns Dynkin labels of weight (coefficients of its decomposition to the sum of fundamental weights) ";
dynkinLabels[rs_?rootSystemQ][wg_?weightQ]:=2*(#.wg)/(#.#)& /@ rs[simpleRoots];

Expect["Dynkin labels of sl(3) root", True, Module[{rs=makeSimpleRootSystem[A,2]},dynkinLabels[rs][rs[simpleRoot][1]]=={2,-1}]]

Expect["Dynkin labels of so(5) root", True, Module[{rs=makeSimpleRootSystem[B,2]},dynkinLabels[rs][rs[simpleRoot][1]]=={2,-2}]]

orthogonalSubsystem::"usage"=
    "orthogonalSubsystem[rs_?rootSystemQ,subs_?rootSystemQ] returns subset of positive roots of root system rs, which 
    are orthogonal to simple roots of subsystem subs";
orthogonalSubsystem[rs_?rootSystemQ,subs_?rootSystemQ]:=Cases[positiveRoots[rs], z_ /; And @@ (z.#==0& /@ subs[simpleRoots])]

Expect["Subsistem, orthogonal to highest root of so(5)", True, Module[{b2=makeSimpleRootSystem[B,2],a1},a1=makeFiniteRootSystem[{highestRoot[b2]}]; orthogonalSubsystem[b2,a1]==makeFiniteRootSystem[{b2[simpleRoot][1]}]]]


projection::"usage"=
    "projection[rs_?rootSystemQ][weights__?weightQ] projects given weight to the root (sub)system\n
    projection[rs_?rootSystemQ][fe_formalElement] projects the formal elements";

projection[rs_finiteRootSystem][{weights__?weightQ}]:= 
    Map[Function[w,(Inverse[cartanMatrix[rs]]. ( 2*(w.#/(#.#))& /@ rs[simpleRoots])).rs[simpleRoots]],{weights}]

Expect["Projection to A_1 \\subset B_2",True, 
       Module[{b2=makeSimpleRootSystem[B,2],a1},
	      a1=makeFiniteRootSystem[{highestRoot[b2]}]; 
	      projection[a1][rho[b2]]==a1[simpleRoot][1]]]

projection[rs_affineRootSystem][{weights__?weightQ}]:= 
    Map[makeAffineWeight[projection[rs[finiteRootSystem]][#[finitePart]],#[level],#[grade]]&,{weights}]

Expect["Projection to A_1 \\subset B_2 for affine algebras",True, 
       Module[{b2a=makeAffineExtension[ makeSimpleRootSystem[B,2]],a1a},
	      a1a=makeAffineExtension[makeFiniteRootSystem[{highestRoot[b2]}]]; 
	      projection[a1][rho[b2a]]==a1[simpleRoot][1] && 
	      projection[a1][b2a[imaginaryRoot]]==a1a[imaginaryRoot]]]

projection[rs_?rootSystemQ][wg_?weightQ]:=projection[rs][{wg}][[1]];

(*    Map[Apply[Plus,#]&,Outer[(#1.#2)/(#2.#2)*#2&,{weights},rs[simpleRoots]]] *)

formalElement::"usage"=
    "Datastructure to represent formal elements of the ring of characters (linear combinations of formal exponents of weights).\n
    Internally the data is held in hashtable.\n
    fe_formalElement[weight_?weightQ] returns multiplicity of a given weight (coefficient in front of Exp[weight])\n
    fe_formalElement[weights] returns list of weights\n
    fe_formalElement[multiplicities] returns list of multiplicities
    formalElements can be added, multiplied by number, Exp[wg_?weightQ] and by formaElements\n
    fe_formalElement[hashtable] returns formalElement's data as hashtable\n
    "

(* formalElement/:fe_formalElement[weight_?weightQ]/;NumberQ[fe[[1]][weight]]:=fe[[1]][weight];*)

formalElement/:fe_formalElement[weight_?weightQ]:=If[hasKey[fe[[1]],weight], fe[[1]][weight], 0]

formalElement/:fe_formalElement[weights]:=keys[fe[[1]]];

formalElement/:fe_formalElement[multiplicities]:=values[fe[[1]]];

formalElement/:x_formalElement==y_formalElement:=x[weights]==y[weights] && x[multiplicities]==y[multiplicities];

makeFormalElement::"usage"=
    "makeFormalElement[{weights___?weightQ},{multiplicities___?NumberQ}] creates formal element with given weights and corresponding multiplicities\n
    makeFormalElement[{weights__?weightQ}] creates formal element where weight multiplicity is calculated as the number of appearances of weight in arguments list"

makeFormalElement[{weights___?weightQ},{multiplicities___?NumberQ}]:=formalElement[makeHashtable[{weights},{multiplicities}]];

Expect["Formal element construction", 2, makeFormalElement[{makeFiniteWeight[{1,2}],makeFiniteWeight[{3,4}]},{2,3}][makeFiniteWeight[{1,2}]]]

makeFormalElement[{weights__?weightQ}]:=
    Module[{res},
	   res=makeFormalElement[makeHashtable[{},{}]];
	   Scan[(res[hashtable][#]=res[#]+1)&,{weights}];
	   res];

Expect["Formal element construction", 2, makeFormalElement[{makeFiniteWeight[{1,2}],makeFiniteWeight[{3,4}], makeFiniteWeight[{1,2}]}][makeFiniteWeight[{1,2}]]]

makeFormalElement[h_]:=formalElement[h];

Expect["Formal element construction", True, 
       makeFormalElement[{makeFiniteWeight[{1,2}],makeFiniteWeight[{3,4}]},{2,3}]==
       makeFormalElement[makeHashtable[{makeFiniteWeight[{1,2}],makeFiniteWeight[{3,4}]},{2,3}]]]

subElement::"usage"=
    "subElement[fe_formalElement,{weights___?weightQ}] creates formalElement with the given subset of weights";
subElement[fe_formalElement,{weights___?weightQ}]:=makeFormalElement[{weights},fe/@{weights}];

Expect["subElement",{1},subElement[makeFormalElement[positiveRoots[makeSimpleRootSystem[B,2]],{1,2,3,4}],{makeFiniteWeight[{1,-1}]}][multiplicities]]

formalElement/:fe_formalElement[hashtable]:=fe[[1]];

Expect["Formal element to hastable conversion", True, 
       Module[{fe=makeFormalElement[{makeFiniteWeight[{1,2}],makeFiniteWeight[{3,4}]},{2,3}]},
	      keys[fe[hashtable]]==fe[weights] && values[fe[hashtable]]==fe[multiplicities]]]


formalElement/:x_formalElement + y_formalElement:=Module[{res},
							 res=makeFormalElement[makeHashtable[{},{}]];
							 Scan[(res[hashtable][#]:=x[#]+y[#])&,Union[x[weights],y[weights]]];
							 res];

Expect["Formal element addition", 5, 
       (makeFormalElement[{makeFiniteWeight[{1,2}],makeFiniteWeight[{3,4}]},{2,3}]+makeFormalElement[{makeFiniteWeight[{1,2}],makeFiniteWeight[{3,5}]},{3,3}])[makeFiniteWeight[{1,2}]]]

formalElement/:x_formalElement*n_?NumberQ:=makeFormalElement[x[weights],n*x[multiplicities]];

Expect["Formal element multiplication by number", 6, 
       (3*makeFormalElement[{makeFiniteWeight[{1,2}],makeFiniteWeight[{3,4}]},{2,3}])[makeFiniteWeight[{1,2}]]]

formalElement/:x_formalElement*Exp[w_?weightQ]:=
    Module[{ws},
	   ws=Select[(#+w)&/@x[weights],checkGrade[x]];
	   makeFormalElement[ws,(x[#-w])&/@ws]]

Expect["Formal element multiplication by exponent of weight", 2, 
       (Exp[makeFiniteWeight[{1,1}]]*makeFormalElement[{makeFiniteWeight[{1,2}],makeFiniteWeight[{3,4}]},{2,3}])[makeFiniteWeight[{2,3}]]]

formalElement/:x_formalElement * y_formalElement:=Plus @@ ((y[#]*(x*Exp[#]))& /@ y[weights]);

Expect["Formal elements multiplication", True,
       Exp[makeFiniteWeight[{1,1}]]*makeFormalElement[{makeFiniteWeight[{1,2}],makeFiniteWeight[{3,4}]},{2,3}]==
       makeFormalElement[{makeFiniteWeight[{1,1}]}]*makeFormalElement[{makeFiniteWeight[{1,2}],makeFiniteWeight[{3,4}]},{2,3}]]

(* formalElement/:orbit[rs_?rootSystemQ][fe_formalElement]:= *)

projection[rs_?rootSystemQ][fe_formalElement]:=
    Module[{res},
	   res=makeFormalElement[makeHashtable[{},{}]];
	   Scan[(res[hashtable][(projection[rs][{#}])[[1]]]=res[(projection[rs][{#}])[[1]]]+fe[#])&,fe[weights]];
	   res];

Expect["Projection for formal elements", 2, 
       Module[{b2=makeSimpleRootSystem[B,2],a1},
	      a1=makeFiniteRootSystem[{highestRoot[b2]}];
	      makeFormalElement[projection[a1]/@ Flatten[orbit[b2][weight[b2][1,1]]]][makeFiniteWeight[{1,1}]]]]

regularSubalgebra::"usage"=
    "regularSubalgebra[rs_finiteRootSystem][rootIndices__?NumberQ] returns root system of regular subalgebra obtained by removing all unlisted nodes \n
    from Dynkn diagram of the algebra";
regularSubalgebra[rs_finiteRootSystem][rootIndices__?NumberQ]:=makeFiniteRootSystem[rs[simpleRoot]/@ {rootIndices}];

Expect["Regular subalgebra B2 of B4", True, regularSubalgebra[makeSimpleRootSystem[B,4]][3,4]==makeFiniteRootSystem[{makeFiniteWeight[{0,0,1,-1}],makeFiniteWeight[{0,0,0,1}]}]]

simpleBranching::"usage"=
    "Calculate branching coefficients with simple algorithm, which constructs all the modules of subalgebra with Freudenthal formula";
simpleBranching[rs_?rootSystemQ,subs_?rootSystemQ][highestWeight_?weightQ]:=
    Module[{mults,pmults,rh=rho[subs],res,wgs},
	   mults=makeFormalElement[freudenthalMultiplicities[rs][highestWeight]];
	   Scan[(mults[hashtable][#]=mults[toFundamentalChamber[rs][#]])&,Flatten[orbit[rs][mults[weights]]]];
	   pmults=projection[subs][mults];
	   res=makeFormalElement[makeHashtable[{},{}]];
	   wgs=Select[Sort[pmults[weights],#1.rh>#2.rh&],mainChamberQ[subs]];
	   Scan[(res[hashtable][#]=pmults[#];pmults=pmults - pmults[#]*makeFormalElement[freudenthalMultiplicities[subs][#]])&, wgs];
	   res];

Expect["Simple branching", True, 
       Module[{b4=makeSimpleRootSystem[B,4],b2,wg},
	      b2=regularSubalgebra[b4][3,4];
	      wg=weight[b4][0,1,0,2];
	      Sort[simpleBranching[b4,b2][wg][multiplicities]]=={6,10,19,30,40,60}]]


anomalousWeights::"usage"="
    anomalousWeights[rs_?rootSystemQ][hweight_?weightQ] returns the formal element, consisting of anomalous weights";
anomalousWeights[rs_?rootSystemQ][hweight_?weightQ]:=
    makeFormalElement @@ Transpose[{#[[1]]-rho[rs],#[[2]]}& /@ orbitWithEps[rs][hweight+rho[rs]]]


fan::"usage"=
    "Constructs fan of the embedding";
fan[rs_?rootSystemQ,subs_?rootSystemQ]:=
    Module[{pr,r,roots},
	   roots=Complement[positiveRoots[rs],orthogonalSubsystem[rs,subs]];
	   pr=makeFormalElement[projection[subs][roots]] - makeFormalElement[positiveRoots[subs]];
	   Fold[Expand[#1*(1-Exp[#2])^(pr[#2])]&,makeFormalElement[{zeroWeight[subs]}],pr[weights]]];
(* !!!!!!!!!!!!!!!!!!!!!                      ^^^^^^^^ This can be negative *)

getOrderedWeightsProjectedToWeylChamber[{algroots__?weightQ},subs_?rootSystemQ,hweight_?weightQ]:=
    Module[{rh=rho[subs]},
	   Sort[
	       Union[Flatten[weightSystem[Select[projection[subs][{algroots}],(#.rh>=0)&]][projection[subs][{hweight}][[1]]]]],
	       #1.rh>#2.rh&]];


extendedAnomElement[rs_?rootSystemQ,subs_?rootSystemQ][highestWeight_?weightQ]:=
    Module[{anomW,selW,selWM,rh=rho[rs],orth,ortrh},
	   orth=orthogonalSubsystem[rs,subs];
	   ortrh=rho[orth];
	   anomW=anomalousWeights[rs][highestWeight];
	   selW=Select[anomW[weights],Function[x,mainChamberQ[orth][x+rh-projection[subs][x+rh]]]];
	   selWM=makeFormalElement[projection[subs][selW],(anomW[#]*dimension[orth][#+rh-ortrh])&/@selW];
	   selWM];

ourBranching[rs_?rootSystemQ,subs_?rootSystemQ][highestWeight_?weightQ]:=
    Module[{anomW,selW,selWM,fn,reprw,orth,res,toFC,rh,subrh,gamma0,sgamma0,hw},
	   orth=orthogonalSubsystem[rs,subs];
	   selWM=extendedAnomElement[rs,subs][highestWeight];

	   rh=rho[rs];
	   subrh=rho[subs];
	   hw=Sort[selWM[weights],#1.subrh>#2.subrh&][[1]];
	   reprw=getOrderedWeightsProjectedToWeylChamber[positiveRoots[rs],subs,hw];
	   fn=fan[rs,subs];

	   gamma0=Sort[fn[weights],#1.subrh<#2.subrh&][[1]];
	   sgamma0=fn[gamma0];
	   fn=fn*Exp[-gamma0];
	   def=-projection[subs][rh];
	   toFC=Function[z,Module[{tmp=toFundamentalChamberWithParity[subs][z-def]},{tmp[[1]]+def,tmp[[2]]}]];
	   res=makeHashtable[reprw,Table[0,{Length[reprw]}]];
	   insideQ:=NumberQ[res[toFC[#][[1]]]]&;
	   Scan[Function[v,
			 res[v]=-1/sgamma0*(
			     -selWM[v-gamma0]+
			     Plus@@(fn[weights] /. x_?weightQ :> If[insideQ[v+x],fn[x]*res[toFC[v+x][[1]]]*toFC[v+x][[2]],0]));
			],
		reprw];
	   makeFormalElement[keys[res],values[res]]
	  ]


Expect["Our branching", True, 
       Module[{b4=makeSimpleRootSystem[B,4],b2,wg},
	      b2=regularSubalgebra[b4][3,4];
	      wg=weight[b4][0,1,0,2];
	      Union[ourBranching[b4,b2][wg][multiplicities]]=={0,6,10,19,30,40,60}]]


branching2[rs_?rootSystemQ,subs_?rootSystemQ][highestWeight_?weightQ]:=
    Module[{anomW,selW,selWM,fn,reprw,orth,res,toFC,rh,subrh,gamma0,sgamma0,hw},
	   orth=orthogonalSubsystem[rs,subs];
	   selWM=extendedAnomElement[rs,subs][highestWeight];

	   rh=rho[rs];
	   subrh=rho[subs];
	   hw=Sort[selWM[weights],#1.subrh>#2.subrh&][[1]];
	   reprw=Sort[
	       Flatten[orbit[subs][getOrderedWeightsProjectedToWeylChamber[positiveRoots[rs],subs,hw]]],
	       #1.subrh>#2.subrh&];

	   fn=fan[rs,subs];

	   gamma0=Sort[fn[weights],#1.subrh<#2.subrh&][[1]];
	   sgamma0=fn[gamma0];

	   fn=fn*Exp[-gamma0];
	   

	   def=-projection[subs][rh];
	   res=makeHashtable[reprw,Table[0,{Length[reprw]}]];
	   insideQ:=NumberQ[res[#]]&;
	   Scan[Function[v,
			 res[v]=-1/sgamma0*(
			     -selWM[v-gamma0]+
			     Plus@@(fn[weights] /. x_?weightQ :> If[insideQ[v+x],fn[x]*res[v+x],0]));
			],
		reprw];
	   makeFormalElement[keys[res],values[res]]
	  ]

Expect["branching2", True, 
       Module[{b4=makeSimpleRootSystem[B,4],b2,wg,fe, mcw},
	      b2=regularSubalgebra[b4][3,4];
	      wg=weight[b4][0,1,0,2];
	      fe=branching2[b4,b2][wg];
	      mcw=Select[fe[weights],mainChamberQ[b2]];
	      Union[fe/@mcw]=={0,6,10,19,30,40,60}]]


drawPlaneProjection[axe1_,axe2_,f_formalElement]:=
    Graphics[(Text[f[#],{#[standardBase][[axe1]],#[standardBase][[axe2]]}] &) /@ f[weights]]

draw3dProjection[axe1_,axe2_,axe3_,f_formalElement]:=
    Graphics3D[(Text[f[#],{#[standardBase][[axe1]],#[standardBase][[axe2]], #[standardBase][[axe3]]}]) & /@ f[weights]]

End[]

EndPackage[]