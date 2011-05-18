AppendTo[$Path,$InitialDirectory <> "/../src/"];

<<affine.m;

b4=Subscript[B,4];
al=b4[simpleRoots];
b={al[[3]],al[[4]]};
b2=makeFiniteRootSystem[b];

f=fan[b4,b2];

Export["/home/anton/programing/Affine/tests/fan.png",Graphics[Text[f[#],{#[standardBase][[3]],#[standardBase][[4]]}] & /@ f[weights]]]

Out[5]= /home/anton/programing/Affine/tests/fan.png

wg=weight[b4][0,1,0,2]; (* makeFiniteWeight[{2,2,1,1}] *)
aw=projection[b2][anomalousWeights[b4][wg]];

Export["/home/anton/programing/Affine/tests/anom.png",Graphics[Text[aw[#],{#[standardBase][[3]],#[standardBase][[4]]}] & /@ aw[weights]]]

Out[23]= /home/anton/programing/Affine/tests/anom.png

Out[20]= /home/anton/programing/Affine/tests/anom.png


extendedAnomElement[rs_?rootSystemQ,subs_?rootSystemQ][highestWeight_?weightQ]:=
    Module[{anomW,selW,selWM,rh=rho[rs],orth,ortrh},
	   orth=orthogonalSubsystem[rs,subs];
	   ortrh=rho[orth];
	   anomW=anomalousWeights[rs][highestWeight];
	   selW=Select[anomW[weights],Function[x,mainChamberQ[orth][x+rh-projection[subs][x+rh]]]];
	   selWM=makeFormalElement[projection[subs][selW],(anomW[#]*dimension[orth][#+rh-ortrh])&/@selW];
	   selWM];


eae=extendedAnomElement[b4,b2][wg];

Part::partw: Part 2 of formalElement[table$655] does not exist.

Part::partw: Part 2 of formalElement[table$655] does not exist.

                 7  5  3  1
finiteWeight[4, {-, -, -, -}]
                 2  2  2  2

Export["/home/anton/programing/Affine/tests/eanom.png",Graphics[Text[eae[#],{#[standardBase][[3]],#[standardBase][[4]]}] & /@ eae[weights]]]


br=ourBranching[b4,b2][wg+rho[b4]]

finiteWeight[4, {0, 0, -2, -2}]1

Out[34]= formalElement[table$911]

Export["/home/anton/programing/Affine/tests/brc.png",Graphics[Text[br[#],{#[standardBase][[3]],#[standardBase][[4]]}] & /@ br[weights]]]


br0=simpleBranching[b4,b2][wg]


Export["/home/anton/programing/Affine/tests/brc0.png",Graphics[Text[br0[#],{#[standardBase][[3]],#[standardBase][[4]]}] & /@ br0[weights]]]

Out[54]= /home/anton/programing/Affine/tests/brc.png


ort=orthogonalSubsystem[b4,b2]

dimension[ort][makeFiniteWeight[{1,-1,0,0}]]

dimension[makeSimpleRootSystem[B,2]][makeFiniteWeight[{1,-1}]]


dimension[{makeFiniteWeight[{1,1}]}][makeFiniteWeight[{2,2}]]

b2

