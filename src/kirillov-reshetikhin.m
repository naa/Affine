BeginPackage["KirillovReshetikhin`"]

t::"usage"="t[n] gives t-number n, i.e. 1+t+t^2+...+t^(n-1)";
tFactorial::"usage"="tFactorial[n] gives t-analogue of the factorial, that is product t[1]*t[2]*...*t[n]";
tBinomial::"usage"="tBinomial[n,k] gives t-analogue of the binomial coefficient";
gradedTensorProductMultiplicity::"usage"="gradedTensorProductMultiplicity[l_List,n_List,C_List] \n 
computes graded tensor product decomposition coefficient\n
 of the irreducible reprentation with the highest weight with Dynkin labels l\n
 in the tensor product of Kirillov-Reshetikhin modules with the indices n\n
 of the algebra with the Cartan matrix C.\n\n
For example, gradedTensorProductMultiplicity[{0,0},{{3,0}},{{2,-1},{-1,2}}] will compute\n
the multiplicity of the trivial (scalar) representation of the algebra sl(3) (A_3) \n
in the graded decomposition of the cube of first fundamental representation.\n
gradedTensorProductMultiplicity[{0,0},{{0,0},{3,0}},{{2,-1},{-1,2}}] will do the same in the cube of representation with the highest weight with Dynkin labels [2,0]
    ";


tensorProductMultiplicity::"usage"="tensorProductMultiplicity[l_List,n_List,C_List] \n 
computes tensor product decomposition coefficient\n
 of the irreducible reprentation with the highest weight with Dynkin labels l\n
 in the tensor product of Kirillov-Reshetikhin modules with the indices n\n
 of the algebra with the Cartan matrix C.\n\n
For example, tensorProductMultiplicity[{0,0},{{3,0}},{{2,-1},{-1,2}}] will compute\n
the multiplicity of the trivial (scalar) representation of the algebra sl(3) (A_3) \n
in the decomposition of the cube of first fundamental representation.\n
tensorProductMultiplicity[{0,0},{{0,0},{3,0}},{{2,-1},{-1,2}}] will do the same in the cube of representation with the highest weight with Dynkin labels [2,0]
 ";
Begin["`Private`"]

t[n_?IntegerQ]:=1+Sum[t^i,{i,n-1}];
tFactorial[n_?IntegerQ]/;n<0:=ComplexInfinity;
tFactorial[n_?IntegerQ]/;n>=0:=Product[t[i],{i,n}]


tBinomial[0,0]:=0;
tBinomial[n_?IntegerQ,0]:=1;
tBinomial[n_?IntegerQ,k_?IntegerQ]/;n<0||k<0:=0;
tBinomial[n_?IntegerQ,k_?IntegerQ]:=tFactorial[n]/(tFactorial[k]* tFactorial[n-k]);

myIP[0]:={{0}};
myIP[n_]:=IntegerPartitions[n];

gradedTensorProductMultiplicity[l_List,n_List,C_List]:=Module[
    {ms=Map[Tally,#]&/@myIP/@(Inverse[C].(Sum[j*n[[j]],{j,Length[n]}]-l)),elemf},
    elemf[m__]:=Module[{p,ml={m},tpart},
		       p[a_,i_]:=Sum[Min[i,j]*n[[j]][[a]],{j,Length[n]}]
		       -Sum[C[[a,b]]*
			    Sum[Min[i,ml[[b]][[j]][[1]]]*ml[[b]][[j]][[2]],
				{j,Length[ml[[b]]]}],
			    {b,Length[C]}];
		       tpart=t^Sum[1/2*C[[a,b]]*
				   Sum[
				       Min[ml[[a]][[i]][[1]],ml[[b]][[j]][[1]]]*ml[[a]][[i]][[2]]*ml[[b]][[j]][[2]],
				       {i,1,Length[ml[[a]]]},
				       {j,1,Length[ml[[b]]]}],
				   {a,Length[C]},{b,Length[C]}];
		       (*Print["ml: ",ml," p: ",p," tp: ",tpart];*)
		       tpart*Product[Product[
			   If[ml[[a]][[j]][[1]]>0,
			      tBinomial[p[a,ml[[a]][[j]][[1]]]+ml[[a]][[j]][[2]],
					ml[[a]][[j]][[2]]],
			      1],
			   {j,Length[ml[[a]]]}],{a,Length[C]}]];
    Plus@@(Flatten[Outer[elemf,Sequence@@ms,1]])]

tensorProductMultiplicity[l_List,n_List,C_List]:=gradedTensorProductMultiplicity[l,n,C]/.{t->1}

End[]

EndPackage[]