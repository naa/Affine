AppendTo[$Path,$InitialDirectory <> "/../src/"];

<<affine.m;

b4=Subscript[B,4];
al=b4[simpleRoots];
b={al[[3]],al[[4]]};
b2=makeFiniteRootSystem[b];

f=fan[b4,b2];

Export["/home/anton/programing/Affine/tests/graph.png",Graphics[Text[f[#],{#[standardBase][[3]],#[standardBase][[4]]}] & /@ f[weights]]]

wg=weight[b4][0,1,0,2]; (* makeFiniteWeight[{2,2,1,1}] *)
aw=projection[b2][anomalousWeights[b4][wg]];

Export["/home/anton/programing/Affine/tests/anom.png",Graphics[Text[aw[#],{#[standardBase][[3]],#[standardBase][[4]]}] & /@ aw[weights]]]

Out[23]= /home/anton/programing/Affine/tests/anom.png

Out[20]= /home/anton/programing/Affine/tests/anom.png

extendedAnomElement[rs_?rootSystemQ,subs_?rootSystemQ][highestWeight_?weightQ]:=
    Module[{anomW,selW,selWM,fn,reprw,orth,res,toFC,rh,subrh,gamma0,sgamma0},
	   orth=orthogonalSubsystem[rs,subs];
	   anomW=anomalousWeights[rs][highestWeight];
	   selW=Select[anomW[weights],mainChamberQ[orth]]; (* <---- !!!!!! *)
	   selWM=makeFormalElement[projection[subs][selW],(anomW[#]*dimension[orth][#])&/@selW];
	   {selW,selWM}]





eae=extendedAnomElement[b4,b2][wg][[2]];

Export["/home/anton/programing/Affine/tests/eanom.png",Graphics[Text[eae[#],{#[standardBase][[3]],#[standardBase][[4]]}] & /@ eae[weights]]]

Out[63]= /home/anton/programing/Affine/tests/eanom.png

Out[60]= /home/anton/programing/Affine/tests/eanom.png

br=ourBranching[b4,b2][wg+rho[b4]]

Export["/home/anton/programing/Affine/tests/brc.png",Graphics[Text[br[#],{#[standardBase][[3]],#[standardBase][[4]]}] & /@ br[weights]]]

br0=simpleBranching[b4,b2][wg]

Out[53]= formalElement[table$1422]

Export["/home/anton/programing/Affine/tests/brc0.png",Graphics[Text[br0[#],{#[standardBase][[3]],#[standardBase][[4]]}] & /@ br0[weights]]]

Out[54]= /home/anton/programing/Affine/tests/brc.png

Out[51]= /home/anton/programing/Affine/tests/brc.png

rho[b4]

                          7  5  3  1
Out[49]= finiteWeight[4, {-, -, -, -}]
                          2  2  2  2

Out[47]= /home/anton/programing/Affine/tests/brc.png

{finiteWeight[4, {0, 0, 2, -2}], finiteWeight[4, {0, 0, -1, -2}], 
 
>    finiteWeight[4, {0, 0, 2, 0}], finiteWeight[4, {0, 0, -2, 0}], 
 
>    finiteWeight[4, {0, 0, 2, 1}], finiteWeight[4, {0, 0, 0, -2}], 
 
>    finiteWeight[4, {0, 0, 0, -1}], finiteWeight[4, {0, 0, 0, 0}], 
 
>    finiteWeight[4, {0, 0, -2, 1}], finiteWeight[4, {0, 0, 1, -1}], 
 
>    finiteWeight[4, {0, 0, -1, 2}], finiteWeight[4, {0, 0, -1, -1}], 
 
>    finiteWeight[4, {0, 0, -1, 1}], finiteWeight[4, {0, 0, 1, -2}], 
 
>    finiteWeight[4, {0, 0, -1, 0}], finiteWeight[4, {0, 0, 2, 2}], 
 
>    finiteWeight[4, {0, 0, -2, -1}], finiteWeight[4, {0, 0, 2, -1}], 
 
>    finiteWeight[4, {0, 0, 1, 0}], finiteWeight[4, {0, 0, 0, 2}], 
 
>    finiteWeight[4, {0, 0, 1, 2}], finiteWeight[4, {0, 0, -2, -2}], 
 
>    finiteWeight[4, {0, 0, 1, 1}], finiteWeight[4, {0, 0, 0, 1}], 
 
>    finiteWeight[4, {0, 0, -2, 2}]}{1, -4, 6, 6, -4, 6, -24, 36, -4, 16, -4, 
 
>    16, 16, -4, -24, 1, -4, -4, -24, 6, -4, 1, 16, -24, 1}
finiteWeight[4, {0, 0, -2, -2}]1

Out[46]= formalElement[table$1242]

Out[45]= /home/anton/programing/Affine/tests/eanom.png

Out[26]= /home/anton/programing/Affine/tests/eanom.png

ort=orthogonalSubsystem[b4,b2]

Out[28]= {finiteWeight[4, {1, -1, 0, 0}], finiteWeight[4, {0, 1, 0, 0}], 
 
>    finiteWeight[4, {1, 0, 0, 0}], finiteWeight[4, {1, 1, 0, 0}]}

dimension[ort][makeFiniteWeight[{1,-1,0,0}]]

         14
Out[29]= --
         3

dimension[makeSimpleRootSystem[B,2]][makeFiniteWeight[{1,-1}]]


dimension[{makeFiniteWeight[{1,1}]}][makeFiniteWeight[{2,2}]]

b2

Out[37]= finiteRootSystem[2, 4, 
 
>    {finiteWeight[4, {0, 0, 1, -1}], finiteWeight[4, {0, 0, 0, 1}]}]

Out[35]= 10

Out[34]= -1

Out[33]= -5

         14
Out[30]= --
         3

?Times