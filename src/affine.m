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

emptyRootSystem::"usage"=
    "represents trivial (empty) root system";

makeFiniteRootSystem::"usage"=
    "makeFiniteRootSystem[{roots__finiteWeight}] creates finiteRootSystem structure from the List of simple roots\n
    makeFiniteRootSystem[{roots__finiteWeight}] creates  finiteRootSystem structure from the List of simple roots\n
    given as the lists of coordinates";

makeAffineRootSystem::"usage"=
    "makeAffineRootSystem[roots__List] creates affine root system which extends finite root system specified by simple roots 'roots'";


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
F::"usage"="represents root systems of series F";
G::"usage"="represents root systems of series G";

weightQ::"usage"=
    "Weight predicate"

checkGrade::"usage"=
    "computations for affine Lie algebras are limited by some grade, by default it is equal to 10. To change it set rs[gradeLimit].\n
    checkGrade[rs_?rootSystemQ][w_?weightQ] checks if given weight has absolute value of grade less then gradeLimit. Returns true 
    for finite-dimensional case";

gradeLimit::"usage"=
    "Limits computation for affine Lie algebras, i.e. b2a=OverHat[Subscript[B,2]]; b2a[gradeLimit]=15";

reflection::"usage"=
    "reflection[x_finiteWeight] represents reflection in the hyperplane, orthogonal to the given root x\n
    reflection[x_affineWeight] represents affine reflection in the hyperplane, orthogonal to the given root x\n
    reflection[x][y] reflects y in the hyperplane, orthogonal to x"

coroot::"usage"="returns coroot of given root"

cartanMatrix::"usage"=
    "cartanMatrix[rs_?rootSystemQ] returns Cartan matrix of root system rs";

rootSystemQ::"usage"=
    "Predicate for root system data structures";

weylGroupElement::"usage"=
    "weylGroupElement[x__Integer][rs_?rootSystemQ] represents element of Weyl group composed of basic reflecetions x \n
    of root system rs. Example: weylGroupElement[1,2,1][makeSimpleRootSystem[B,2]] = s1 s2 s1";

fundamentalWeights::"usage"=
    "fundamentalWeights[rs_?rootSystemQ] calculates fundamental weights of given root system rs";

rho::"usage"=
    "rho[rs_?rootSystemQ] - Weyl vector of root system rs (sum of fundamental weights)";

toFundamentalChamber::"usage"=
    "toFundamentalChamber[rs_?rootSystemQ][vec_?weightQ] acts on a weight vec by simple reflections of rs till it gets to main Weyl chamber";

mainChamberQ::"usage"=
    "mainChamberQ[rs_?rootSystemQ][wg_?weightQ] tells if weights wg lies inside the main Weyl chamber of root system rs or not";

partialOrbit::"usage"=
    "partialOrbit[rs_finiteRootSystem][{weights__finiteWeight}] constructs Weyl partial orbit of given set of weights. \n
    It starts with the list of weights and reflects them away from main Weyl chamber.\n
    For w in fundamental chamber it gives the whole orbit.";

orbit::"usage"=
    "orbit[rs_?rootSystemQ][wg_?weightQ] returns the weight of Weyl orbit, containing given weight wg. \n
    Orbit is given as the list of lists starting with the weight in dominant Weyl chamber\n
    orbit[rs_?rootSystemQ][{wg_?weightQ}] works for a list of weights";

orbitWithEps::"usage"="returns orbit with the determinants of Weyl reflections";

formalOrbit::"usage"="returns the same thing as orbitWithEps, but as formalElement";

positiveRoots::"usage"=
    "positiveRoots[rs_?rootSystemQ] returns positive roots of root system rs";

highestRoot::"usage"="returns highest root of root system";

makeAffineExtension::"usage"=
    "makeAffineExtension[fs_finiteRootSystem] creates root system of affine Lie algebra which 
    is extension of given finite-dimensional root system";

OverHat::"usage"=
    "OverHat[rs_finiteRootSystem] creates affine extension of root system rs, it is equivalent makeAffineExtension[rs], but uses mathematical notation in notebook interface";

affineRootSystem::"usage"=
    "affineRootSystem[rank_Integer,finiteRootSystem_finiteRootSystem,imaginaryRoot_affineWeight,realRoots_List] 
    represents root system of affine Lie algebra.\n
    affineRootSystem[rank] returns rank of the root system\n
    affineRootSystem[imaginaryRoot] returns imaginary (zeroth) root\n
    affineRootSystem[realRoots] returns list of real roots (roots of finite-dimensional subalgebra)
    affineRootSystem[dimension] returns the dimension of the space where the roots are realized as the vectors\n
    affineRootSystem[simpleRoots] returns unsorted list of simple roots in the root system.\n
    affineRootSystem[simpleRoot][n_Integer] returns n'th simple root"

imaginaryRoot::"usage"="Symbol used to get imaginary root of affine Lie algebra";

zeroWeight::"usage"="zeroWeight[rs_?rootSystemQ] returns zero root of root system rs (zero of rs[dimension]-dimemsional space)"; 

marks::"usage"="marks[rs_affineRootSystem] returns marks of affine Lie algebra";

comarks::"usage"="comarks[rs_affineRootSystem] returns comarks of affine Lie algebra";

weight::"usage"=
    "weight[rs_?rootSystemQ][labels__Integer] constructs weight defined by Dynkin labels";

dynkinLabels::"usage"=
    "dynkinLabels[rs_?rootSystemQ][wg_?weightQ] returns Dynkin labels of weight (coefficients of its decomposition to the sum of fundamental weights) ";

orthogonalSubsystem::"usage"=
    "orthogonalSubsystem[rs_?rootSystemQ,subs_?rootSystemQ] returns subset of positive roots of root system rs, which 
    are orthogonal to simple roots of subsystem subs";

projection::"usage"=
    "projection[rs_?rootSystemQ][weights__?weightQ] projects given weight to the root (sub)system\n
    projection[rs_?rootSystemQ][fe_formalElement] projects the formal elements";


formalElement::"usage"=
    "Datastructure to represent formal elements of the ring of characters (linear combinations of formal exponents of weights).\n
    Internally the data is held in hashtable.\n
    fe_formalElement[weight_?weightQ] returns multiplicity of a given weight (coefficient in front of Exp[weight])\n
    fe_formalElement[weights] returns list of weights\n
    fe_formalElement[multiplicities] returns list of multiplicities
    formalElements can be added, multiplied by number, Exp[wg_?weightQ] and by formaElements\n
    fe_formalElement[hashtable] returns formalElement's data as hashtable\n
    "

hashtable::"usage"=
    "hashtable[fe_formalElement] or fe[hashtable] returns weights and multiplicities of formal element as a hash table";

makeFormalElement::"usage"=
    "makeFormalElement[{weights___?weightQ},{multiplicities___?NumberQ}] creates formal element with given weights and corresponding multiplicities\n
    makeFormalElement[{weights__?weightQ}] creates formal element where weight multiplicity is calculated as the number of appearances of weight in arguments list"


subElement::"usage"=
    "subElement[fe_formalElement,{weights___?weightQ}] creates formalElement with the given subset of weights";

parabolicSubalgebra::"usage"=
    "parabolicSubalgebra[rs_finiteRootSystem][rootIndices__?NumberQ] returns root system of regular subalgebra obtained by removing all unlisted nodes \n
    from Dynkn diagram of the algebra";

module::"usage"=
    "module[rs_?rootSystemQ,singWeights_formalElement,subs_?rootSystemQ,limit_?NumberQ] represents Lie algebra module \n
    generated by the set of singular weights with the additional symmetry of root system subs, \n
    limit limits number of subtraction in the construction of infinite-dimensional modules"

makeModule::"usage"=
    "makeModule[rs_?rootSystemQ][singWeights_formalElement,subs_?rootSystemQ|emptyRootSystem,opts___?OptionQ] is general construction for modules";

makeVermaModule::"usage"="makeVermaModule[rs_?rootSystemQ][hw_?weightQ,opts___?OptionQ] constructs Verma module. \n
    Option 'limit' limits construction with the number of simple root subtractions. By default it is chosen in such a way\n
    that Verma module includes corresponding irreducible module";

makeParabolicVermaModule::"usage"=
    "makeParabolicVermaModule[rs_?rootSystemQ][hw_?weightQ,{symgen___Integer},opts___?OptionQ] constructs parabolic Verma module. \n
    {symgen} is the list of root indices of parabolic subalgbra. \n
    Option 'limit' limits construction with the number of simple root subtractions. By default it is chosen in such a way\n
    that Verma module includes corresponding irreducible module";

makeIrreducibleModule::"usage"=
    "makeIrreducibleModule[rs_?rootSystemQ][hw_?weightQ] constructs irreducible module";
    
dimension::"usage"=
    "dimension[rs_?rootSystemQ][hweight_?weightQ] returns dimension of Lie algebra highest weight module with the highest weight hweight";

weightSystem::"usage"=
    "weightSystem[rs_?rootSystemQ][highestWeight_?weightQ] returns the set of dominant weights in the highest weight module. \n
    The list is split in pieces by number of root substractions\n
    weightSystem[hm_highestWeightModule] returns the set of dominant weights in the highest weight module. \n
    The list is split in pieces by number of root substractions";

freudenthalMultiplicities::"usage"=
    "freudenthalMultiplicities[rs_finiteRootSystem][highestWeight_finiteWeight] returns hashtable with the multiplicities of 
    weights in the highest weight module";

racahMultiplicities::"usage"=
    "racahMultiplicities[rs_?rootSystemQ] returns hashtable with the multiplicities of 
    weights in the highest weight module. It uses recurrent relation similar to Racah formula";


singularElement::"usage"="
    singularElement[m_module] returns the formal element, consisting of singular weights of module hm";

character::"usage"=
    "character[m_module] returns character of module as formalElement datastructure";

branching::"usage"=
    "branching[m_module,subs_?rootSystemQ] returns branching coefficients of decomposition of module m to \n
    the irreducible modules of subalgebra defined by root system subs";

simpleBranching::"usage"="Temporary function";
ourBranching::"usage"="Temporary function";
branching2::"usage"="Temporary function";

tensorProduct::"usage"=
    "tensorProduct[m__module] returns module which is tensor product of modules";

CircleTimes::"usage"=CircleTimes::"usage" <> "\n denotes tensor product of modules";

directSum::"usage"=
    "directSum[ms__module] returns module which is direct sum of modules";

CirclePlus::"usage"=CirclePlus::"usage" <> "\n denotes direct sum of modules";

decomposion::"usage"=
    "Tensor product decomposition"

fan::"usage"=
    "fan[rs_?rootSystemQ, subs_?rootSystemQ] constructs fan of the embedding";

ourBranching::"usage"=
    "Calculate branching coefficients with alternative algorithm";

branching2::"usage"=
    "Calculate branching coefficients with alternative algorithm without folding to main Weyl chamber";

simpleBranching::"usage"=
    "Calculate branching coefficients with simple algorithm, which constructs all the modules of subalgebra with Freudenthal formula";

drawPlaneProjection::"usage"=
    "drawPlaneProjection[axe1_,axe2_,f_formalElement] draws projection to the plane of axe1, axe2 (e.g. 1,2 or 3,4, or 1,4 etc) of some formal element
    \n(branching, multiplicities, tensor product coefficients)";

draw3dProjection::"usage"=
    "draw3dProjection[axe1_,axe2_,axe3_,f_formalElement] draws projection to the 3d space of axe1, axe2, axe3 (e.g. 1,2,3 or 1,3,4 etc) of some formal element
    \n(branching, multiplicities, tensor product coefficients)"

stringFunction::"usage"=
    "stringFunciton[rs_affineRootSystem][hweigth_affineWeight,wg_affineWeight] returns power series decomposition of string function"

branchingFunction::"usage"=
    "branchingFunciton[rs_affineRootSystem,subs_affineRootSystem][hweigth_affineWeight,wg_affineWeight] returns power series decomposition of branching function"

weights::"usage"="weights[fe_formalElement] or fe[weights] returns list of weights of formal element";
multiplicities::"usage"="multiplicities[fe_formalElement] or fe[multiplicities] returns list of weight multiplicities in formalElement. List is in the same order as fe[weights] ";

q::"usage"="formal variable for string and branching funcitons";


embeddingIndex::"usage"="Calculate index of the embedding";

stringFunctions::"usage"="stringFunctions[rs_affineRootSystem,{ls__?NumberQ}] returns string functions of highest weight irreducible module"

branchingFunctions::"usage"="branchingFunctions[rs_affineRootSystem,subs_affineRootSystem,{ls__?NumberQ}] returns string functions of highest weight irreducible module"

textPlot::"usage"=
    "textPlot[m_module] plots module multiplicities";

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
	   (*table[_]=Null; *)
	   (table[First[#]]=Last[#])&/@Transpose[{keys,values}];
	   table]
(*
	   DownValues[table]=((table[#[[1]]] -> #[[2]] &)/@ Transpose[{keys,values}]);
	   table]
*)
makeFiniteRootSystem[{roots__finiteWeight}]:=finiteRootSystem[Length[{roots}],{roots}[[1]][dimension],{roots}]

makeFiniteRootSystem[{roots__List}]:=makeFiniteRootSystem[makeFiniteWeight/@{roots}]

finiteRootSystem/:x_finiteRootSystem[rank]:=x[[1]];
finiteRootSystem/:x_finiteRootSystem[dimension]:=x[[2]];
finiteRootSystem/:x_finiteRootSystem[simpleRoots]:=x[[3]];
finiteRootSystem/:x_finiteRootSystem[simpleRoot][0]:=highestRoot[x];
finiteRootSystem/:x_finiteRootSystem[simpleRoot][n_Integer]:=x[[3]][[n]];
finiteRootSystem/:x_finiteRootSystem[finiteRootSystem]=x;

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
makeSimpleRootSystem[E,rank_Integer]/; (rank>=3) && (rank<=8) :=makeFiniteRootSystem[Append[{1/2*{1,-1,-1,-1,-1,-1,-1,1},
								   {1,1,0,0,0,0,0,0}},
								  Table[{If [ j==i-2, 1, If [j==j-2, -1, 0]]},{i,3,rank},{j,1,8}]]];
makeSimpleRootSystem[F,4]:=makeFiniteRootSystem[{{1,-1,0,0},{0,1,-1,0},{0,0,1,0},1/2*{1,-1,-1,-1}}];
makeSimpleRootSystem[G,2]:=makeFiniteRootSystem[{{1,-1,0},{-2,1,1}}];
							   
(*
elif L='E' and r>2 and r<9 then
    S:=
      seq(cat('e',j)-cat('e',j+1),j=i..i+r-3),
      cat('e',i+r-2)+cat('e',i+r-3),
      (-cat('e',i)-cat('e',i+1)-cat('e',i+2)-cat('e',i+3)-cat('e',i+4)
       -cat('e',i+5)-cat('e',i+6)-cat('e',i+7))/2; i:=i+8-r
  elif R='F4' then
    S:=
      cat('e',i)-cat('e',i+1),
      cat('e',i+1)-cat('e',i+2),
      cat('e',i+2),
      (-cat('e',i)-cat('e',i+1)-cat('e',i+2)+cat('e',i+3))/2
  elif R='G2' then
    S:=-2*cat('e',i)+cat('e',i+1)+cat('e',i+2),cat('e',i)-cat('e',i+1)
*)

Subscript[A,n_Integer]=makeSimpleRootSystem[A,n];
Subscript[B,n_Integer]=makeSimpleRootSystem[B,n];
Subscript[C,n_Integer]=makeSimpleRootSystem[C,n];
Subscript[D,n_Integer]=makeSimpleRootSystem[D,n];
Subscript[E,n_Integer]=makeSimpleRootSystem[E,n];
Subscript[F,n_Integer]=makeSimpleRootSystem[F,n];
Subscript[G,n_Integer]=makeSimpleRootSystem[G,n];

affineRootSystem/:rs_affineRootSystem[gradeLimit]=10;

finiteRootSystem/:rs_finiteRootSystem[gradeLimit]=10;

checkGrade[rs_][w_finiteWeight]=True;
checkGrade[rs_][w_affineWeight]:=(Abs[w[grade]]<=rs[gradeLimit]) /. rs[gradeLimit]->10;
checkGrade[gr_?NumberQ][w_affineWeight]:=Abs[w[grade]]<=gr;

weightQ[x_finiteWeight]=True;
weightQ[x_affineWeight]=True;
weightQ[_]=False;

reflection[x_?weightQ]:=Function[y, y-2*(x.y)/(x.x)*x];

coroot[x_?weightQ]:=2*x/(x.x);

cartanMatrix[rs_?rootSystemQ]:=Transpose[Outer[Dot,rs[simpleRoots],coroot/@rs[simpleRoots]]];

revApply[x_,f_]:=f[x];

rootSystemQ[rs_]:=MatchQ[rs,x_finiteRootSystem|x_affineRootSystem]

weylGroupElement[rs_?rootSystemQ][x__Integer]:=Function[z,Fold[revApply,z,reflection /@ rs[simpleRoot]/@{x}]];

fundamentalWeights[rs_finiteRootSystem]:=Plus@@(Inverse[cartanMatrix[rs]]*rs[simpleRoots]);

rho[rs_?rootSystemQ]:=Plus@@fundamentalWeights[rs];

rho[{posroots__?weightQ}]:=1/2*(Plus@@{posroots});

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


mainChamberQ[{roots___?weightQ}][wg_?weightQ]:=And@@(#.wg>=0&/@(coroot/@ Select[{roots},#.#=!=0&]));
mainChamberQ[rs_?rootSystemQ][wg_?weightQ]:=mainChamberQ[rs[simpleRoots]][wg];

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

orbit[rs_?rootSystemQ][weight_?weightQ]:=partialOrbit[rs][{toFundamentalChamber[rs][weight]}];
orbit[rs_?rootSystemQ][{weights___?weightQ}]:=partialOrbit[rs][toFundamentalChamber[rs] /@ {weights}];

positiveRoots[rs_finiteRootSystem]:=Map[-#&,Flatten[partialOrbit[rs][Map[-#&,rs[simpleRoots]]]]]

(*dimension[{pr__?weightQ}][hweight_?weightQ]:=*)
dimension[{}][hweight_]:=1;
dimension[{pr__}][hweight_]:=
    Module[{rh=rho[{pr}]},
	   Times@@((  (#.(hweight+rh))/(rh.#) )&/@{pr})];

dimension[rs_?rootSystemQ][hweight_?weightQ]:=dimension[positiveRoots[rs]][toFundamentalChamber[rs][hweight]];

weightSystem[{posroots__?weightQ},{initialWeights__?weightQ}, mainChamberPredicate_, gradePredicate_]:=
    Module[{minusPosRoots=-{posroots}},
	   Flatten[NestWhileList[Function[x,Complement[
	       Cases[Flatten[Outer[Plus,minusPosRoots,x]],y_/;
		     And[gradePredicate[y],mainChamberPredicate[y]]]
	       ,x]],{initialWeights},#=!={}&]]];

weightSystem[{posroots__?weightQ},{initialWeights__?weightQ}, mainChamberPredicate_]:=
    Module[{mgrade=Max[grade/@{posroots}]},
	   weightSystem[{posroots},{initialWeights},mainChamberPredicate,checkGrade[mgrade]]];

weightSystem[{posroots__?weightQ},{initialWeights__?weightQ}]:=weightSystem[{posroots},{initialWeights},mainChamberQ[{posroots}]];

weightSystem[{posroots__?weightQ}][highestWeight_?weightQ]:=weightSystem[{posroots},{highestWeight},mainChamberQ[{posroots}]];

weightSystem[rs_?rootSystemQ][highestWeight_?weightQ]:=weightSystem[positiveRoots[rs],{highestWeight},mainChamberQ[rs]];

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
	   makeFormalElement[mults]];

(* freudenthalMultiplicities[makeSimpleRootSystem[B,2]][makeFiniteWeight[{5,5}]] *)
orbitWithEps[rs_?rootSystemQ][weight_?weightQ]:=Flatten[MapIndexed[Function[{x,i},Map[{#,(-1)^(i[[1]]+1)}&,x]],orbit[rs][weight]],1];

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
	   makeFormalElement[mults]]


(* racahMultiplicities[makeSimpleRootSystem[B,2]][makeFiniteWeight[{5,5}]] *)

(* b2=makeSimpleRootSystem[B,2];
   rh=rho[b2];
   rs=b2; 

   fan=Map[{rh-#[[1]],#[[2]]}&,Rest[orbitWithEps[rs][rh]]]; *)

highestRoot[rs_finiteRootSystem]:=toFundamentalChamber[rs][rs[simpleRoot][Ordering[(#.#&)/@rs[simpleRoots],-1][[1]]]]

makeAffineExtension[fs_finiteRootSystem]:=affineRootSystem[fs[rank],fs,makeAffineWeight[-highestRoot[fs],0,1],(makeAffineWeight[#,0,0]&)/@fs[simpleRoots]]

OverHat[rs_finiteRootSystem]:=makeAffineExtension[rs];

affineRootSystem/:rs_affineRootSystem[rank]:=rs[[1]]

affineRootSystem/:rs_affineRootSystem[finiteRootSystem]:=rs[[2]]

affineRootSystem/:rs_affineRootSystem[realRoots]:=rs[[4]]

affineRootSystem/:rs_affineRootSystem[imaginaryRoot]:=rs[[3]]

affineRootSystem/:rs_affineRootSystem[simpleRoots]:=Prepend[rs[[4]],rs[[3]]]

affineRootSystem/:rs_affineRootSystem[simpleRoot][0]:=rs[[3]];
affineRootSystem/:rs_affineRootSystem[simpleRoot][n_?NumberQ]/;n<=rs[rank]:=rs[simpleRoots][[n+1]];

zeroWeight[rs_finiteRootSystem]:=makeFiniteWeight[Table[0,{rs[dimension]}]];
zeroWeight[rs_affineRootSystem]:=makeAffineWeight[zeroWeight[rs[finiteRootSystem]],0,0];

(* Ugly hack *)
positiveRoots[rs_affineRootSystem]:=Join[Map[-#&,Flatten[partialOrbit[rs][Map[-#&,rs[simpleRoots]]]]],
					 Join@@Table[Rest[NestWhileList[#+makeAffineWeight[zeroWeight[rs[finiteRootSystem]],0,1]&,zeroWeight[rs],checkGrade[rs][#]&]],{rs[rank]}]];

toFundamentalChamber[rs_affineRootSystem][vec_affineWeight]:=
    First[NestWhile[Function[v,
			     reflection[Scan[If[#.v<0,Return[#]]&,rs[simpleRoots]]][v]],
		    vec,
		    Head[#]=!=reflection[Null]&]]

marks[rs_affineRootSystem]:=Prepend[Inverse[cartanMatrix[rs[finiteRootSystem]]].(-2*#.rs[simpleRoot][0]/(#.#)&)/@rs[realRoots],1]

comarks[rs_affineRootSystem]:=Module[{cm},cm=marks[rs]*Map[#.#/2&,rs[simpleRoots]]; cm/(GCD @@ cm)*(GCD @@ marks[rs])];

fundamentalWeights[rs_affineRootSystem]:=Map[makeAffineWeight[#[[1]],#[[2]],0]&,
					     Transpose[{Prepend[fundamentalWeights[rs[finiteRootSystem]],
								zeroWeight[rs[finiteRootSystem]]],
							comarks[rs]}]]

weight[rs_?rootSystemQ][labels__Integer]:=fundamentalWeights[rs].{labels}

dynkinLabels[rs_?rootSystemQ][wg_?weightQ]:=2*(#.wg)/(#.#)& /@ rs[simpleRoots];

orthogonalSubsystem[rs_?rootSystemQ,subs_?rootSystemQ]:=Cases[positiveRoots[rs], z_ /; z-projection[subs][z]==z]

(*And @@ (z.#==0& /@ subs[simpleRoots])]*)

projection[rs_finiteRootSystem][{weights__?weightQ},ei_:1]:= 
    Map[Function[w,(Inverse[cartanMatrix[rs]]. ( 2*(w.#/(#.#))& /@ rs[simpleRoots])).rs[simpleRoots]],{weights}]

projection[rs_affineRootSystem][{weights__?weightQ},ei_:1]:= 
    Map[makeAffineWeight[projection[rs[finiteRootSystem]][#[finitePart]],#[level],#[grade]]&,{weights}]

projection[rs_?rootSystemQ][wg_?weightQ,ei_:1]:=projection[rs][{wg},ei][[1]];

(* formalElement/:fe_formalElement[weight_?weightQ]/;NumberQ[fe[[1]][weight]]:=fe[[1]][weight];*)

formalElement/:fe_formalElement[weight_?weightQ]:=If[hasKey[fe[[1]],weight], fe[[1]][weight], 0]

formalElement/:fe_formalElement[weights]:=keys[fe[[1]]];

formalElement/:fe_formalElement[multiplicities]:=values[fe[[1]]];

formalElement/:x_formalElement==y_formalElement:=x[weights]==y[weights] && x[multiplicities]==y[multiplicities];

makeFormalElement[{weights___?weightQ},{multiplicities___?NumberQ}]:=
    Module[{ht=makeHashtable[{},{}]},
	   Scan[If[NumberQ[ht[#[[1]]]], ht[#[[1]]]=ht[#[[1]]]+#[[2]],ht[#[[1]]]=#[[2]]]&,
		Transpose[{{weights},{multiplicities}}]];
	   makeFormalElement[ht]];

makeFormalElement[{weights__?weightQ}]:=
    Module[{res},
	   res=makeFormalElement[makeHashtable[{},{}]];
	   Scan[(res[hashtable][#]=res[#]+1)&,{weights}];
	   res];

makeFormalElement[h_]:=formalElement[h];

subElement[fe_formalElement,{weights___?weightQ}]:=makeFormalElement[{weights},fe/@{weights}];

formalElement/:fe_formalElement[hashtable]:=fe[[1]];

hashtable[fe_formalElement]=fe[hashtable];

formalElement/:x_formalElement + y_formalElement:=Module[{res},
							 res=makeFormalElement[makeHashtable[{},{}]];
							 Scan[(res[hashtable][#]:=x[#]+y[#])&,Union[x[weights],y[weights]]];
							 res];

formalElement/:x_formalElement*n_?NumberQ:=makeFormalElement[x[weights],n*x[multiplicities]];

formalElement/:x_formalElement*Exp[w_?weightQ]:=
    Module[{ws},
	   ws=Select[(#+w)&/@x[weights],checkGrade[x]];
	   makeFormalElement[ws,(x[#-w])&/@ws]]

formalElement/:x_formalElement * y_formalElement:=Plus @@ ((y[#]*(x*Exp[#]))& /@ y[weights]);

orbitWithEps[rs_?rootSystemQ][weight_?weightQ]:=Flatten[MapIndexed[Function[{x,i},Map[{#,(-1)^(i[[1]]+1)}&,x]],orbit[rs][weight]],1];

formalOrbit[rs_?rootSystemQ][weight_?weightQ]:=makeFormalElement @@ Transpose[Flatten[MapIndexed[Function[{x,i},Map[{#,(-1)^(i[[1]]+1)}&,x]],orbit[rs][weight]],1]];

formalElement/:orbit[rs_?rootSystemQ][fe_formalElement]:=
    Plus @@ (fe[#]*formalOrbit[rs][#]&/@fe[weights]);

formalElement/:noSignOrbit[rs_?rootSystemQ][fe_formalElement]:=
    Module[{res=makeHashtable[{},{}],t},
	   Scan[Function[w, Scan[(res[#]=fe[w])&,Flatten[orbit[rs][{w}]]]], fe[weights]];
	   makeFormalElement[res]];

(*    Plus @@ (fe[#]*makeFormalElement[Flatten[orbit[rs][#]]]&/@fe[weights]);*)

projection[rs_?rootSystemQ][fe_formalElement,ei_:1]:=
    Module[{res},
	   res=makeFormalElement[makeHashtable[{},{}]];
	   Scan[(res[hashtable][(projection[rs][{#},ei])[[1]]]=res[(projection[rs][{#},ei])[[1]]]+fe[#])&,fe[weights]];
	   res];

parabolicSubalgebra[rs_finiteRootSystem][rootIndices__?NumberQ]:=makeFiniteRootSystem[rs[simpleRoot]/@ {rootIndices}];

simpleBranching[rs_?rootSystemQ,subs_?rootSystemQ][highestWeight_?weightQ]:=
    Module[{mults,pmults,rh=rho[subs],res,wgs},
	   mults=freudenthalMultiplicities[rs][highestWeight];
	   Scan[(mults[hashtable][#]=mults[toFundamentalChamber[rs][#]])&,Flatten[orbit[rs][mults[weights]]]];
	   pmults=projection[subs][mults];
	   res=makeFormalElement[makeHashtable[{},{}]];
	   wgs=Select[Sort[pmults[weights],#1.rh>#2.rh&],mainChamberQ[subs]];
	   Scan[(res[hashtable][#]=pmults[#];pmults=pmults - pmults[#]*freudenthalMultiplicities[subs][#])&, wgs];
	   res];

singularElement[rs_?rootSystemQ][hweight_?weightQ]:=
    makeFormalElement @@ Transpose[{#[[1]]-rho[rs],#[[2]]}& /@ orbitWithEps[rs][hweight+rho[rs]]]

fan[rs_?rootSystemQ,subs_?rootSystemQ]:=
    Module[{pr,r,roots,ei},
	   ei=embeddingIndex[rs,subs];
	   roots=Select[projection[subs][positiveRoots[rs],ei],#=!= zeroWeight[subs]&];
	   pr=makeFormalElement[roots] - makeFormalElement[positiveRoots[subs]];
	   r=makeFormalElement[{zeroWeight[subs]}];
	   Scan[Function[w,
			 r=Expand[r*(1-Exp[w])^(pr[w])];
			 r=subElement[r,Select[r[weights],checkGrade[subs]]];
			],
		pr[weights]];
	   r];
(*	   r=Fold[Expand[#1*(1-Exp[#2])^(pr[#2])]&,makeFormalElement[{zeroWeight[subs]}],pr[weights]];
	   subElement[r,Select[r[weights],checkGrade[rs]]]];*)
(* !!!!!!!!!!!!!!!!!!!!!                      ^^^^^^^^ This can be negative *)

(*
fan[rs_?rootSystemQ,subs_?rootSystemQ]:=
    Module[{pr,r,roots},
	   roots=Complement[positiveRoots[rs],orthogonalSubsystem[rs,subs]];
	   pr=makeFormalElement[projection[subs][roots]] - makeFormalElement[positiveRoots[subs]]];
*)	   
(*
module::"usage"=
    "module[rs_?rootSystemQ,singWeights_formalElement,subs_?rootSystemQ,limit_?NumberQ] represents Lie algebra module \n
    generated by the set of singular weights with the additional symmetry of root system subs"
*)
makeModule[rs_?rootSystemQ][singWeights_formalElement,subs_?rootSystemQ|subs_emptyRootSystem,limit_:10]:=
    module[rs,singWeights,subs,limit];

module/:m_module[rootSystem]:=m[[1]];
rootSystem[m_module]=m[rootSystem];

subSystem[m_module]:=m[[3]];

limit[m_module]:=m[[4]];

makeVermaModule[rs_?rootSystemQ][hw_?weightQ,limit_:10]:=makeModule[rs][makeFormalElement[{hw},{1}],emptyRootSystem[],limit];
makeVermaModule[rs_?rootSystemQ][{ls__?NumberQ},limit_:10]:=makeVermaModule[rs][weight[rs][ls],limit];

makeParabolicVermaModule[rs_?rootSystemQ][hw_?weightQ,{symgen___Integer},limit_:10]:=
    makeModule[rs][makeFormalElement[{hw},{1}],If [symgen===Null, emptyRootSystem[], parabolicSubalgebra[rs][symgen]],limit];

makeIrreducibleModule[rs_?rootSystemQ][hw_?weightQ]:=makeModule[rs][makeFormalElement[{hw},{1}],rs,Max[2*Plus@@ (hw.#&/@simpleRoots[rs]),rs[gradeLimit]]]

makeIrreducibleModule[rs_?rootSystemQ][ls__?NumberQ]:=makeIrreducibleModule[rs][weight[rs][ls]];


singularElement[m_module]:=If [subSystem[m]=!=emptyRootSystem[], Exp[-rho[subSystem[m]]]*orbit[subSystem[m]][Exp[rho[subSystem[m]]]*m[[2]]], m[[2]]];

cSingularWeights[m_module]:=m[[2]];

weightSystem[{posroots__?weightQ},{initialWeights__?weightQ}]:=weightSystem[{posroots},{initialWeights},mainChamberQ[{posroots}]];

(*weightSystem[m_module]:=makeFormalElement[weightSystem[positiveRoots[rootSystem[m]],cSingularWeights[m][weights],mainChamberQ[rootSystem[m]]]];
*)

dropWhile = Drop[#, LengthWhile[#, #2]] &;

weightSystem[m_module]:=
    Module[{check,rs=rootSystem[m],res},
	   check=If [subSystem[m]=!=emptyRootSystem[],
		     mainChamberQ[subSystem[m]][#] && checkGrade[rs][#] & , 
		     checkGrade[rs]];
	   res=Select[
	       Flatten[
		   Function[x,
			    x-((Plus @@ (simpleRoots[rs]*#) )& /@ Flatten[Permutations/@IntegerPartitions[#,{Length[simpleRoots[rs]]},Range[0,#]]&/@Range[0,limit[m]],2])]/@cSingularWeights[m][weights]],
	       check];
	   makeFormalElement[res,Table[1,{Length[res]}]]];
(*
weightSystem[m_module]:=Module[{roots,checkLimit,rh,rs=rootSystem[m],res},
			       roots=simpleRoots[rs];
			       rh=rho[rs];
			       checkLimit=And[rh.#>=-limit[m],checkGrade[rs][#]]&;
			       res=Union[Flatten[
				   FixedPointList[
				       Function[x,
						Select[Function[y,
							 Module[{rts=dropWhile[roots,#.y<0&]},
								If[Length[rts]>0 && checkLimit[y-First[rts]],y-First[rts]]]]/@ x, #=!=Null&]],
				       cSingularWeights[m][weights]]]];
			       res=If [subSystem[m]=!=emptyRootSystem[],Select[res,mainChamberQ[subSystem[m]]],res];
			       makeFormalElement[res,Table[1,{Length[res]}]]];
*)
(*
weightSystem[m_module]:=Module[{minusRoots, checkChamber, rs=rootSystem[m],res,rh},
			       minusRoots=-simpleRoots[rootSystem[m]];
			       Print[minusRoots];
			       rh=rho[rootSystem[m]];
			       checkChamber=If [subSystem[m]=!=emptyRootSystem[],
						rh.#>=-limit[m] && mainChamberQ[subSystem[m]][#] && checkGrade[rs][#] & , 
						And[rh.#>=-limit[m],checkGrade[rs][#]]&];
			       Print[checkChamber/@m[[2]][weights]];
			       res=Flatten[NestWhileList[Function[x,Complement[
				   Select[Flatten[Outer[Plus,minusRoots,x]],
					  checkChamber],
				   x]],m[[2]][weights],#=!={}&]];
			       makeFormalElement[res,Table[1,{Length[res]}]]];
*)
partialCharacter[m_module]:=
    Module[{rs=rootSystem[m],subs=subSystem[m],toFC,rh,mults,aw,wgs,fan,tmp,gamma0},
	   rh=rho[rs];
	   fan=makeFormalElement @@ Transpose[Map[{rh-#[[1]],#[[2]]}&,Rest[orbitWithEps[rs][rh]]]];
(*	   fan=singularElement[rs][zeroWeight[rs]];
	   gamma0=Sort[fan[weights],#1.rh>#2.rh&][[1]];
	   fan=fan*Exp[-gamma0];*)

	   If [subs=!=emptyRootSystem[],
	       toFC=toFundamentalChamber[subs],
	       toFC=#&;
	      ];
	   wgs=Sort[weightSystem[m][weights],#1.rh>#2.rh&];
	   aw=singularElement[m];
	   mults=makeHashtable[{},{}];
	   insideQ=IntegerQ[mults[toFC[#]]]&;
	   Scan[Function[v,
			 mults[v]=
			 Plus @@ Map[ If [insideQ[v+#], -fan[#]*mults[toFC[v+#]], 0]&, fan[weights]];
			 If [IntegerQ[aw[v]], mults[v]=mults[v]+aw[v]];
			],
		wgs];
	   makeFormalElement[mults]];

(*character[m_module]:=If [subSystem[m]=!=emptyRootSystem[], character[makeModule[rootSystem[m]][singularElement[m],emptyRootSystem[]]]]*)

character[m_module]:=If [subSystem[m]=!=emptyRootSystem[], noSignOrbit[subSystem[m]][partialCharacter[m]], partialCharacter[m]]

directSum[ms__module]:=makeModule[rootSystem[{ms}[[1]]]][Plus @@ (singularElement/@{ms}),emptyRootSystem[]];

CirclePlus[ms__module]=directSum[ms];

simpleBranching[m_module,subs_?rootSystemQ]:=
    Module[{mults,pmults,rh=rho[subs],res,wgs},
	   mults=character[m];
	   Scan[(mults[hashtable][#]=mults[toFundamentalChamber[rs][#]])&,Flatten[orbit[rs][mults[weights]]]];
	   pmults=projection[subs][mults];
	   res=makeFormalElement[makeHashtable[{},{}]];
	   wgs=Select[Sort[pmults[weights],#1.rh>#2.rh&],mainChamberQ[subs]];
	   Scan[(res[hashtable][#]=pmults[#];pmults=pmults - pmults[#]*freudenthalMultiplicities[subs][#])&, wgs];
	   res];

tensorProduct[m1_module,m2_module]/; Head[rootSystem[m1]]==finiteRootSystem && Head[rootSystem[m2]]==finiteRootSystem:=
    Module[{rs,subs=emptyRootSystem[],subroots={},aw,sw1,sw2,nw},
	   rs=CirclePlus[rootSystem[m1],rootSystem[m2]];
	   If [subSystem[m1]=!=emptyRootSystem[],
	       subroots=Join[subroots,Map[appendZeros[rootSystem[m2][dimension],#]&,subSystem[m1][simpleRoots]]]];
	   If [subSystem[m2]=!=emptyRootSystem[],
	       subroots=Join[subroots,Map[prependZeros[rootSystem[m1][dimension],#]&,subSystem[m2][simpleRoots]]]];
	   If [Length[subroots]>0,
	       subs=makeFiniteRootSystem[subroots],
	       subs=emptyRootSystem[]];
	   sw1=cSingularWeights[m1];
	   sw2=cSingularWeights[m2];
	   aw=makeFormalElement @@ Transpose @@ Outer[{makeFiniteWeight[Join[#1[standardBase],#2[standardBase]]],sw1[#1]*sw2[#2]}&,sw1[weights],sw2[weights]];
	   makeModule[rs][aw,subs,limit->limit[m1]*limit[m2]]];


(*tensorProduct[m1_module,m2_module]:=Module[{rs,subs=emptyRootSystem[],subroots={},aw},
					   rs=CirclePlus[rootSystem[m1],rootSystem[m2]];
					   If [subSystem[m1]=!=emptyRootSystem[] && subSystem[m2]=!=emptyRootSystem[], subs=CirclePlus[subSystem[m1],subSystem[m2]],
					       If [subSystem[m1]=!=emptyRootSystem[],
						   If [Head[subSystem[m1]]===finiteRootSystem,
						       subs=makeFiniteRootSystem[Map[appendZeros[rootSystem[m2][finiteRootSystem][dimension],#]&,subSystem[m1][finiteRootSystem][simpleRoots]]],
						       subs=makeAffineExtension[makeFiniteRootSystem[Map[appendZeros[rootSystem[m2][finiteRootSystem][dimension],#]&,subSystem[m1][finiteRootSystem][simpleRoots]]]]],
						   If [Head[subSystem[m2]]===finiteRootSystem,
						       subs=makeFiniteRootSystem[Map[prependZeros[rootSystem[m1][finiteRootSystem][dimension],#]&,subSystem[m2][finiteRootSystem][simpleRoots]]],
						       subs=makeAffineExtension[makeFiniteRootSystem[Map[prependZeros[rootSystem[m1][finiteRootSystem][dimension],#]&,subSystem[m2][finiteRootSystem][simpleRoots]]]]]]];
					   aw=Outer[Join[#1],cSingularWeights[m1],cSingularWeights[m2]
						       
							    


finiteRootSystem/:CirclePlus[x_finiteRootSystem,y_finiteRootSystem]:=makeFiniteRootSystem[Join[Map[appendZeros[y[dimension],#]&,x[simpleRoots]],
											       Map[prependZeros[x[dimension],#]&,y[simpleRoots]]]];

affineRootSystem/:CirclePlus[x_affineRootSystem,y_affineRootSystem]:=makeAffineExtension[CirclePlus[x[finiteRootSystem],y[finiteRootSystem]]];
*)

CircleTimes[m1_module,m2_module]=tensorProduct[m1,m2];

embeddingIndex[rs_?rootSystemQ,subs_?rootSystemQ]:=
					     ((#.#)& @ projection[subs[finiteRootSystem]][highestRoot[rs[finiteRootSystem]]])/
					     ((#.#)& @ highestRoot[subs[finiteRootSystem]]);


extendedSingElement[m_module,subs_?rootSystemQ]:=
    Module[{rs=rootSystem[m],singW,selW,selWM,rh,orth,ortrh,ei},
	   rh=rho[rs];
	   ei=embeddingIndex[rs,subs];
	   orth=orthogonalSubsystem[rs,subs];
	   ortrh=If[orth=!={},rho[orth], zeroWeight[rs]];
	   singW=singularElement[m];
	   selW=Select[singW[weights],mainChamberQ[orth][#+rh-projection[subs][#+rh,ei]]&];
	   selWM=makeFormalElement[projection[subs][selW,ei],(singW[#]*dimension[orth][#+rh-ortrh])&/@selW];
	   selWM];

getOrderedWeightsProjectedToWeylChamber[{algroots__?weightQ},subs_?rootSystemQ,{hws__?weightQ},ei_?NumberQ]:=
    Module[{rh=rho[subs]},
	   Sort[
	       Union[Flatten[
		   weightSystem[Select[projection[subs][{algroots},ei],(#.rh>=0)&], projection[subs][{hws},ei]]]],
 	       #1.rh>#2.rh&]];

ourBranching[m_module,subs_?rootSystemQ]:=
    Module[{singW,selW,selWM,fn,reprw,orth,res,toFC,rh,subrh,gamma0,sgamma0,hw,rs=rootSystem[m],ei},
	   ei=embeddingIndex[rs,subs];
	   orth=orthogonalSubsystem[rs,subs];
	   selWM=extendedSingElement[m,subs];
	   rh=rho[rs];
	   subrh=rho[subs];

	   fn=fan[rs,subs];
	   gamma0=Sort[Select[fn[weights],grade[#]==0&],#1.subrh<#2.subrh&][[1]];
	   sgamma0=fn[gamma0];
	   fn=fn*Exp[-gamma0];
	   reprw=Select[getOrderedWeightsProjectedToWeylChamber[positiveRoots[rs],subs,(cSingularWeights[m]*Exp[-gamma0])[weights],ei],mainChamberQ[subs]];
	   toFC=Function[z,Module[{tmp=toFundamentalChamberWithParity[subs][z+subrh]},{tmp[[1]]-subrh,tmp[[2]]}]];
	   res=makeHashtable[reprw,Table[0,{Length[reprw]}]];
	   insideQ:=NumberQ[res[toFC[#][[1]]]]&;
	   Scan[Function[v,
(*			 Print[v];*)
			 res[v]=-1/sgamma0*(
			     -selWM[v-gamma0]+
			     Plus@@(fn[weights] /. x_?weightQ :> If[insideQ[v+x],fn[x]*res[toFC[v+x][[1]]]*toFC[v+x][[2]],0]));
			],
		reprw];
	   makeFormalElement[keys[res],values[res]]
	  ]

branching::"usage"=
    "branching[m_module,subs_?rootSystemQ] returns branching coefficients of decomposition of module m to \n
    the irreducible modules of subalgebra defined by root system subs";





decomposion::"usage"=
    "Tensor product decomposition"

getOrderedWeightsProjectedToWeylChamber[{algroots__?weightQ},subs_?rootSystemQ,hweight_?weightQ]:=
    Module[{rh=rho[subs]},
	   Sort[
	       Union[Flatten[weightSystem[Select[projection[subs][{algroots}],(#.rh>=0)&]][projection[subs][{hweight}][[1]]]]],
	       #1.rh>#2.rh&]];


extendedSingElement[rs_?rootSystemQ,subs_?rootSystemQ][highestWeight_?weightQ]:=
    Module[{singW,selW,selWM,rh=rho[rs],orth,ortrh},
	   orth=orthogonalSubsystem[rs,subs];
	   ortrh=rho[orth];
	   singW=singularElement[rs][highestWeight];
	   selW=Select[singW[weights],Function[x,mainChamberQ[orth][x+rh-projection[subs][x+rh]]]];
	   selWM=makeFormalElement[projection[subs][selW],(singW[#]*dimension[orth][#+rh-ortrh])&/@selW];
	   selWM];

ourBranching[rs_?rootSystemQ,subs_?rootSystemQ][highestWeight_?weightQ]:=
    Module[{singW,selW,selWM,fn,reprw,orth,res,toFC,rh,subrh,gamma0,sgamma0,hw},
	   orth=orthogonalSubsystem[rs,subs];
	   selWM=extendedSingElement[rs,subs][highestWeight];

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


branching2[rs_?rootSystemQ,subs_?rootSystemQ][highestWeight_?weightQ]:=
    Module[{singW,selW,selWM,fn,reprw,orth,res,toFC,rh,subrh,gamma0,sgamma0,hw},
	   orth=orthogonalSubsystem[rs,subs];
	   selWM=extendedSingElement[rs,subs][highestWeight];

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

drawPlaneProjection[axe1_,axe2_,f_formalElement,opts___?OptionQ]:=
    Graphics[(Text[f[#],{#[standardBase][[axe1]],#[standardBase][[axe2]]}] &) /@ f[weights],opts]

draw3dProjection[axe1_,axe2_,axe3_,f_formalElement]:=
    Graphics3D[(Text[f[#],{#[standardBase][[axe1]],#[standardBase][[axe2]], #[standardBase][[axe3]]}]) & /@ f[weights]]

textPlot[m_module]:=drawPlaneProjection[1,2,character[m],{Axes->False}];
					     

tensorProduct::"usage"=
    "calculate tensor product decomposition coefficients"

stringFunction::"usage"=
    "stringFunciton[rs_affineRootSystem][hweigth_affineWeight,wg_affineWeight] returns power series decomposition of string function"


stringSelector[fe_formalElement,wg_affineWeight,gradeLimit_Integer]:=Plus @@ Table[(q^i)*fe[makeAffineWeight[wg[finitePart],wg[level],-i]],{i,0,gradeLimit}];

stringFunction[rs_affineRootSystem][hweight_affineWeight,wg_affineWeight]:=stringSelector[freudenthalMultiplicities[rs][hweight],wg,rs[gradeLimit]];

branchingFunction[rs_affineRootSystem,subs_affineRootSystem][hweight_affineWeight,wg_affineWeight]:=stringSelector[ourBranching[rs,subs][hweight],wg,rs[gradeLimit]];

(*
    Module[{fe=makeFormalElement[freudenthalMultiplicities[rs][hweight]]},
	   Plus @@ Table[(q^i)*fe[makeAffineWeight[wg[finitePart],wg[level],-i]],{i,0,rs[gradeLimit]}]]*)
(*
branchingFunction::"usage"=
    "branchingFunciton[rs_affineRootSystem,subs_affineRootSystem][hweigth_affineWeight,wg_affineWeight] returns power series decomposition of branching function"
*)

stringFunctions[rs_affineRootSystem,{ls__?NumberQ}]:=
					     Module[{ch,res,zerograde},
						    ch=partialCharacter[makeIrreducibleModule[rs][weight[rs][ls]]];
						    res=makeHashtable[{},{}];
						    zerograde=makeAffineWeight[#[finitePart],level[#],0]&;
						    Scan[Function[x,If[checkGrade[rs][x],
								       If[MemberQ[keys[res],zerograde[x]],
									  res[zerograde[x]]=res[zerograde[x]]+ch[x]*q^(-grade[x]),
									  res[zerograde[x]]=ch[x]*q^(-grade[x])]]],
							 ch[weights]];
						    {dynkinLabels[rs][#],res[#]}&/@keys[res]];
					     

branchingFunctions[rs_affineRootSystem,subs_affineRootSystem,{ls__?NumberQ}]:=
					     Module[{ch,res,zerograde},
						    ch=ourBranching[makeIrreducibleModule[rs][weight[rs][ls]],subs];
						    res=makeHashtable[{},{}];
						    zerograde=makeAffineWeight[#[finitePart],level[#],0]&;
						    Scan[Function[x,If[checkGrade[subs][x],
								       If[MemberQ[keys[res],zerograde[x]],
									  res[zerograde[x]]=res[zerograde[x]]+ch[x]*q^(-grade[x]),
									  res[zerograde[x]]=ch[x]*q^(-grade[x])]]],
							 ch[weights]];
						    {dynkinLabels[subs][#],res[#]}&/@keys[res]];


makeAffineRootSystem[roots__List]:=makeAffineExtension[makeFiniteRootSystem[{roots}]];

makeAffineRootSystem[roots__finiteWeight]:=makeAffineExtension[makeFiniteRootSystem[{roots}]];

End[]

EndPackage[]