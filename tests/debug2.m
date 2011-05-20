AppendTo[$Path,$InitialDirectory <> "/../src/"];

<<affine.m;

b4=Subscript[B,4];
al=b4[simpleRoots];
b={al[[3]],al[[4]]};
b2=makeFiniteRootSystem[b];

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

Export["/home/anton/programing/Affine/tests/fan.png",Graphics[Text[f[#],{#[standardBase][[3]],#[standardBase][[4]]}] & /@ f[weights]]]

Out[5]= /home/anton/programing/Affine/tests/fan.png

wg=weight[b4][0,1,0,2]; (* makeFiniteWeight[{2,2,1,1}] *)
aw=projection[b2][anomalousWeights[b4][wg]];

Export["/home/anton/programing/Affine/tests/anom.png",Graphics[Text[aw[#],{#[standardBase][[3]],#[standardBase][[4]]}] & /@ aw[weights]]]

extendedAnomElement[rs_?rootSystemQ,subs_?rootSystemQ][highestWeight_?weightQ]:=
    Module[{anomW,selW,selWM,rh=rho[rs],orth,ortrh},
	   orth=orthogonalSubsystem[rs,subs];
	   ortrh=rho[orth];
	   anomW=anomalousWeights[rs][highestWeight];
	   selW=Select[anomW[weights],Function[x,mainChamberQ[orth][x+rh-projection[subs][x+rh]]]];
	   selWM=makeFormalElement[projection[subs][selW],(anomW[#]*dimension[orth][#+rh-ortrh])&/@selW];
	   selWM];


eae=extendedAnomElement[b4,b2][wg];

Export["/home/anton/programing/Affine/tests/eanom.png",Graphics[Text[eae[#],{#[standardBase][[3]],#[standardBase][[4]]}] & /@ eae[weights],Axes->True]]

subrh=rho[b2]

hw=Sort[eae[weights],#1.subrh>#2.subrh&][[1]]

Out[35]= finiteWeight[4, {0, 0, 4, 4}]

subs=b2

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

Export["/home/anton/programing/Affine/tests/brc.png",Graphics[Text[br[#],{#[standardBase][[3]],#[standardBase][[4]]}] & /@ br[weights],Axes->True]]

Out[78]= /home/anton/programing/Affine/tests/brc.png

Out[75]= /home/anton/programing/Affine/tests/brc.png

Out[27]= /home/anton/programing/Affine/tests/brc.png


br0=simpleBranching[b4,b2][wg]

Export["/home/anton/programing/Affine/tests/brc0.png",Graphics[Text[br0[#],{#[standardBase][[3]],#[standardBase][[4]]}] & /@ br0[weights]]]

Out[54]= /home/anton/programing/Affine/tests/brc.png


ort=orthogonalSubsystem[b4,b2]

dimension[ort][makeFiniteWeight[{1,-1,0,0}]]

dimension[makeSimpleRootSystem[B,2]][makeFiniteWeight[{1,-1}]]


dimension[{makeFiniteWeight[{1,1}]}][makeFiniteWeight[{2,2}]]

b2

