AppendTo[$Path,$InitialDirectory <> "/../src/"];

<<affine.m;

Table[{i*j},{i,10},{j,10}]

makeSimpleRootSystem[E,rank_Integer]/; (rank>=3) && (rank<=8):=True

makeSimpleRootSystem[E,8]

1/2*{1,-1,-1,-1}

f[x_,y_:x]:=x+y

fn[i_]:=NestWhileList[Function[x,x+1],1,#<i&]//Timing

Table[{i,fn[i][[1]]},{i,1000,1010}]

Out[21]= {{1000, 0.020001}, {1001, 0.008001}, {1002, 0.012001}, 
 
>    {1003, 0.012}, {1004, 0.012001}, {1005, 0.012001}, {1006, 0.012001}, 
 
>    {1007, 0.008}, {1008, 0.012001}, {1009, 0.024001}, {1010, 0.016001}}

Export["listtiming2.png",ListPlot[Out[21]]]

Out[22]= listtiming2.png

Out[20]= {{1, 0.}, {2, 0.}, {3, 0.}, {4, 0.}, {5, 0.}, {6, 0.}, {7, 0.}, 
 
>    {8, 0.}, {9, 0.}, {10, 0.}}

Export["listtiming.png",ListPlot[Table[{i*200,fn[i*200][[1]]},{i,200}]]]

Out[26]= listtiming.png

Out[25]= $Aborted

Out[24]= $Aborted

Out[23]= listtiming.png

Out[19]= listtiming.png

Out[18]= listtiming.png

?ListPlot

ListPlot[{y , y , ...}] plots points corresponding to a list of values,
           1   2
     assumed to correspond to x coordinates 1, 2, .... 

     ListPlot[{{x , y }, {x , y }, ...}]
                 1   1     2   2
      plots a list of points with specified x
       and y coordinates. ListPlot[{list , list , ...}]
                                        1      2
         plots several lists of points. 

Out[14]= {0., {1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 
 
>     19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 
 
>     37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 
 
>     55, 56, 57, 58, 59, 60, 61, 62, 63, 64, 65, 66, 67, 68, 69, 70, 71, 72, 
 
>     73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 
 
>     91, 92, 93, 94, 95, 96, 97, 98, 99, 100}}

Out[13]= {0., {1, 2, 3, 4, 5, 6, 7, 8, 9, 10}}


f[4]

Out[9]= 4 + x

         1    1     1     1
Out[7]= {-, -(-), -(-), -(-)}
         2    2     2     2

Out[6]= True

Out[5]= makeSimpleRootSystem[E, 9]

Out[4]= makeSimpleRootSystem[E, 2]

Out[3]= True

Out[1]= {{{1}, {2}, {3}, {4}, {5}, {6}, {7}, {8}, {9}, {10}}, 
 
>    {{2}, {4}, {6}, {8}, {10}, {12}, {14}, {16}, {18}, {20}}, 
 
>    {{3}, {6}, {9}, {12}, {15}, {18}, {21}, {24}, {27}, {30}}, 
 
>    {{4}, {8}, {12}, {16}, {20}, {24}, {28}, {32}, {36}, {40}}, 
 
>    {{5}, {10}, {15}, {20}, {25}, {30}, {35}, {40}, {45}, {50}}, 
 
>    {{6}, {12}, {18}, {24}, {30}, {36}, {42}, {48}, {54}, {60}}, 
 
>    {{7}, {14}, {21}, {28}, {35}, {42}, {49}, {56}, {63}, {70}}, 
 
>    {{8}, {16}, {24}, {32}, {40}, {48}, {56}, {64}, {72}, {80}}, 
 
>    {{9}, {18}, {27}, {36}, {45}, {54}, {63}, {72}, {81}, {90}}, 
 
>    {{10}, {20}, {30}, {40}, {50}, {60}, {70}, {80}, {90}, {100}}}

Hello!

?Integer

f[i_Integer]:=i+1

f[0]

FullForm[x_Integer|x_Rational]

Out[7]//FullForm= 
 
>   Alternatives[Pattern[x, Blank[Integer]], Pattern[x, Blank[Rational]]]

Out[6]= 1

Integer is the head used for integers. 

?dimension

dimension[rs_?rootSystemQ][hweight_?weightQ] returns dimension of Lie algebra
   highest weight representation

Message::name: Message name MessageName[finiteWeight[dimension], usage]
     is not of the form symbol::name or symbol::name::language.
Hello!

finitePart[finiteWeight[2,{1,2}]]

Out[7]= finiteWeight[2, {1, 2}]

Out[6]= {1, 2}

Out[5]= {1, 2}

Out[4]= 2

Out[3]= 2

Out[5]= finiteWeight[2, {1, 2}][dimension]

Out[4]= finiteWeight[2, {1, 2}]

Out[3]= makeFiniteWeight[{1, 2}]

Hello!

Context[formalElement]

?BeginPackage

?Plus

??formalElement

Datastructure to represent formal elements of the ring of characters (linear
   combinations of formal exponents of weights).

    Internally the data is held in hashtable.

    fe_formalElement[weight_?weightQ] returns multiplicity of a given weight
   (coefficient in front of Exp[weight])

    fe_formalElement[weights] returns list of weights

    fe_formalElement[multiplicities] returns list of multiplicities
    formalElements can be added, multiplied by number, Exp[wg_?weightQ] and by
   formaElements

    fe_formalElement[hashtable] returns formalElement's data as hashtable


2+2


(fe_formalElement)[(weight_)?weightQ] := If[hasKey[fe[[1]], weight], 
    fe[[1]][weight], 0]
 
(fe_formalElement)[weights] := keys[fe[[1]]]
 
(fe_formalElement)[multiplicities] := values[fe[[1]]]
 
(fe_formalElement)[hashtable] := fe[[1]]
 
(x_formalElement) == (y_formalElement) ^:= x[weights] == y[weights] && 
    x[multiplicities] == y[multiplicities]
 
(x_formalElement) + (y_formalElement) ^:= 
   Module[{res}, res = makeFormalElement[makeHashtable[{}, {}]]; 
     Scan[(res[hashtable][#1] := x[#1] + y[#1]) & , 
      Union[x[weights], y[weights]]]; res]
 
(x_formalElement)*(n_)?NumberQ ^:= makeFormalElement[x[weights], 
    n*x[multiplicities]]
 
formalElement /: E^(w_)?weightQ*(x_formalElement) := 
    Module[{ws}, ws = Select[(#1 + w & ) /@ x[weights], checkGrade[x]]; 
      makeFormalElement[ws, (x[#1 - w] & ) /@ ws]]
 
(x_formalElement)*(y_formalElement) ^:= 
   Plus @@ (y[#1]*(x*Exp[#1]) & ) /@ y[weights]

Datastructure to represent formal elements of the ring of characters (linear
   combinations of formal exponents of weights).

    Internally the data is held in hashtable.

    fe_formalElement[weight_?weightQ] returns multiplicity of a given weight
   (coefficient in front of Exp[weight])

    fe_formalElement[weights] returns list of weights

    fe_formalElement[multiplicities] returns list of multiplicities
    formalElements can be added, multiplied by number, Exp[wg_?weightQ] and by
   formaElements

    fe_formalElement[hashtable] returns formalElement's data as hashtable



x + y + z represents a sum of terms. 
          It is defined for weights of finite and affine Lie algebras
          Direct sum of finite-dimensional and affine Lie algebras can be
            specified as sum of root systems

BeginPackage["context`"] makes context`
     and System` the only active contexts. 

     BeginPackage["context`", {"need `", "need `", ...}]
                                    1         2
      calls Needs on the need . 
                             i

Out[5]= Global`

Hello!

Throw::nocatch: 
   Uncaught Throw[Formal element construction: GOT UNEXPECTED VALUE 0 INSTEAD
      OF 2, assertion exception] returned to top level.

Out[2]= Hold[Throw[Formal element construction: GOT UNEXPECTED VALUE 0\
 
>      INSTEAD OF 2, assertion exception]]

formalElement/:fe_formalElement[weight_?(hasKey[fe[[1]],#])&]:=fe[[1]][weight];

fe=makeFormalElement[{makeFiniteWeight[{1,1}]},{3}]

fe[makeFiniteWeight[{1,1}]]

Out[5]= 0

Out[4]= formalElement[table$127]

Throw::nocatch: 
   Uncaught Throw[Formal element construction: GOT UNEXPECTED VALUE 0 INSTEAD
      OF 2, assertion exception] returned to top level.

Out[2]= Hold[Throw[Formal element construction: GOT UNEXPECTED VALUE 0\
 
>      INSTEAD OF 2, assertion exception]]


fm=makeFormalElement[{makeFiniteWeight[{1,2}],makeFiniteWeight[{3,4}]},{2,3}]

fm[makeFiniteWeight[{3,4}]]

?DownValues

f[x]=f[x]

f[x]


hasKey[hashtable_,key_]:=hashtable[key]=!=Unevaluated[hashtable[key]]


h=makeHashtable[{a,b,c},{2,3,4}]

h[q]

hasKey[h,a]

Out[20]= True

Out[19]= False

Out[18]= table$132[q]

Out[17]= 3

Out[16]= table$132

Out[14]= f[x]

Out[13]= f[x]

Out[12]= f[x]

Out[11]= f[x]

DownValues[f] gives a list of transformation rules corresponding to all
     downvalues defined for the symbol f. 

Out[9]= 0

Out[8]= 0

fm[multiplicities]

Out[7]= {2, 3}

Out[6]= {finiteWeight[2, {1, 2}], finiteWeight[2, {3, 4}]}

Out[5]= formalElement[table$129]

Out[4]= formalElement[table$128]

Out[3]= 0

Throw::nocatch: 
   Uncaught Throw[Formal element construction: GOT UNEXPECTED VALUE 0 INSTEAD
      OF 2, assertion exception] returned to top level.

Out[2]= Hold[Throw[Formal element construction: GOT UNEXPECTED VALUE 0\
 
>      INSTEAD OF 2, assertion exception]]

b4=makeSimpleRootSystem[B,4];

dimension[b4][3*wg]//Timing

Out[19]= {0.036002, 13755924}

Out[18]= {0.032002, 2772}

Out[17]= 2772

wg=weight[b4][0,1,0,2];

pr=positiveRoots[b4];

dimension[pr][wg]

Out[15]= 2772

Out[11]= 2772

Out[6]= $Aborted

rho[pr]//Timing

                              7  5  3  1
Out[9]= {0., finiteWeight[4, {-, -, -, -}]}
                              2  2  2  2

                         7  5  3  1
Out[8]= finiteWeight[4, {-, -, -, -}]
                         2  2  2  2

rho[b4]

                         7  5  3  1
Out[6]= finiteWeight[4, {-, -, -, -}]
                         2  2  2  2



Plus @@ Table[makeFiniteWeight[{i,i}],{i,120}]//Timing

Out[4]= {0.012001, finiteWeight[2, {7260, 7260}]}

Out[3]= {0., finiteWeight[2, {78, 78}]}

Out[27]= {1.25208, finiteWeight[2, {78, 78}]}

Out[26]= {0.408026, finiteWeight[2, {66, 66}]}

         finiteWeight[2, {66, 66}]
Out[25]= -------------------------
                  Timing

Out[24]= finiteWeight[2, {66, 66}]

Out[23]= finiteWeight[2, {55, 55}]

Out[22]= finiteWeight[2, {45, 45}]

Out[21]= finiteWeight[2, {15, 15}]

finiteWeight/:Plus[wgs__finiteWeight]:=
    makeFiniteWeight[Total[ (#[standardBase]&/@ {wgs})]]




makeFiniteWeight[Total[(#[standardBase]&/@Table[makeFiniteWeight[{i,i}],{i,154}])]]

Out[17]= finiteWeight[2, {11935, 11935}]

Out[16]= {11935, 11935}

Out[15]= {11325, 11325}

Out[14]= {11325, 11325}

Out[13]= {120, 120}

Out[12]= {15, 15}

Out[11]= {{1, 1}, {2, 2}, {3, 3}, {4, 4}, {5, 5}}

Out[10]= {finiteWeight[2, {1, 1}], finiteWeight[2, {2, 2}], 
 
>    finiteWeight[2, {3, 3}], finiteWeight[2, {4, 4}], 
 
>    finiteWeight[2, {5, 5}]}

Out[9]= {{Table[makeFiniteWeight[{i, i}], {i, 5}], 
 
>     {{{i, 1}, {i, 1}, {1, 1}}, makeFiniteWeight[{1, 1}], 
 
>      finiteWeight @@ {Length[{1, 1}], {1, 1}}, 
 
>      {{Length[{1, 1}], 2}, {2, {1, 1}}}, finiteWeight @@ {2, {1, 1}}, 
 
>      finiteWeight[2, {1, 1}]}, 
 
>     {{{i, 2}, {i, 2}, {2, 2}}, makeFiniteWeight[{2, 2}], 
 
>      finiteWeight @@ {Length[{2, 2}], {2, 2}}, 
 
>      {{Length[{2, 2}], 2}, {2, {2, 2}}}, finiteWeight @@ {2, {2, 2}}, 
 
>      finiteWeight[2, {2, 2}]}, 
 
>     {{{i, 3}, {i, 3}, {3, 3}}, makeFiniteWeight[{3, 3}], 
 
>      finiteWeight @@ {Length[{3, 3}], {3, 3}}, 
 
>      {{Length[{3, 3}], 2}, {2, {3, 3}}}, finiteWeight @@ {2, {3, 3}}, 
 
>      finiteWeight[2, {3, 3}]}, 
 
>     {{{i, 4}, {i, 4}, {4, 4}}, makeFiniteWeight[{4, 4}], 
 
>      finiteWeight @@ {Length[{4, 4}], {4, 4}}, 
 
>      {{Length[{4, 4}], 2}, {2, {4, 4}}}, finiteWeight @@ {2, {4, 4}}, 
 
>      finiteWeight[2, {4, 4}]}, 
 
>     {{{i, 5}, {i, 5}, {5, 5}}, makeFiniteWeight[{5, 5}], 
 
>      finiteWeight @@ {Length[{5, 5}], {5, 5}}, 
 
>      {{Length[{5, 5}], 2}, {2, {5, 5}}}, finiteWeight @@ {2, {5, 5}}, 
 
>      finiteWeight[2, {5, 5}]}, 
 
>     {finiteWeight[2, {1, 1}], finiteWeight[2, {2, 2}], 
 
>      finiteWeight[2, {3, 3}], finiteWeight[2, {4, 4}], 
 
>      finiteWeight[2, {5, 5}]}}, 
 
>    Plus @@ {finiteWeight[2, {1, 1}], finiteWeight[2, {2, 2}], 
 
>      finiteWeight[2, {3, 3}], finiteWeight[2, {4, 4}], 
 
>      finiteWeight[2, {5, 5}]}, 
 
>    finiteWeight[2, {1, 1}] + finiteWeight[2, {2, 2}] + 
 
>     finiteWeight[2, {3, 3}] + finiteWeight[2, {4, 4}] + 
 
>     finiteWeight[2, {5, 5}], finiteWeight[2, {3, 3}] + 
 
>     finiteWeight[2, {4, 4}] + finiteWeight[2, {5, 5}] + 
 
>     makeFiniteWeight[finiteWeight[2, {1, 1}][standardBase] + 
 
>       finiteWeight[2, {2, 2}][standardBase]], 
 
>    {{{finiteWeight[2, {1, 1}][standardBase], finiteWeight[2, {1, 1}][[2]], 
 
>       {1, 1}}, {finiteWeight[2, {2, 2}][standardBase], 
 
>       finiteWeight[2, {2, 2}][[2]], {2, 2}}, {1, 1} + {2, 2}, 
 
>      {1 + 2, 1 + 2}, {1 + 2, 3}, {1 + 2, 3}, {3, 3}}, 
 
>     makeFiniteWeight[{3, 3}], finiteWeight @@ {Length[{3, 3}], {3, 3}}, 
 
>     {{Length[{3, 3}], 2}, {2, {3, 3}}}, finiteWeight @@ {2, {3, 3}}, 
 
>     finiteWeight[2, {3, 3}]}, 
 
>    finiteWeight[2, {3, 3}] + finiteWeight[2, {4, 4}] + 
 
>     finiteWeight[2, {5, 5}] + finiteWeight[2, {3, 3}], 
 
>    finiteWeight[2, {3, 3}] + finiteWeight[2, {3, 3}] + 
 
>     finiteWeight[2, {4, 4}] + finiteWeight[2, {5, 5}], 
 
>    finiteWeight[2, {4, 4}] + finiteWeight[2, {5, 5}] + 
 
>     makeFiniteWeight[finiteWeight[2, {3, 3}][standardBase] + 
 
>       finiteWeight[2, {3, 3}][standardBase]], 
 
>    {{{finiteWeight[2, {3, 3}][standardBase], finiteWeight[2, {3, 3}][[2]], 
 
>       {3, 3}}, {finiteWeight[2, {3, 3}][standardBase], 
 
>       finiteWeight[2, {3, 3}][[2]], {3, 3}}, {3, 3} + {3, 3}, 
 
>      {3 + 3, 3 + 3}, {3 + 3, 6}, {3 + 3, 6}, {6, 6}}, 
 
>     makeFiniteWeight[{6, 6}], finiteWeight @@ {Length[{6, 6}], {6, 6}}, 
 
>     {{Length[{6, 6}], 2}, {2, {6, 6}}}, finiteWeight @@ {2, {6, 6}}, 
 
>     finiteWeight[2, {6, 6}]}, 
 
>    finiteWeight[2, {4, 4}] + finiteWeight[2, {5, 5}] + 
 
>     finiteWeight[2, {6, 6}], finiteWeight[2, {6, 6}] + 
 
>     makeFiniteWeight[finiteWeight[2, {4, 4}][standardBase] + 
 
>       finiteWeight[2, {5, 5}][standardBase]], 
 
>    {{{finiteWeight[2, {4, 4}][standardBase], finiteWeight[2, {4, 4}][[2]], 
 
>       {4, 4}}, {finiteWeight[2, {5, 5}][standardBase], 
 
>       finiteWeight[2, {5, 5}][[2]], {5, 5}}, {4, 4} + {5, 5}, 
 
>      {4 + 5, 4 + 5}, {4 + 5, 9}, {4 + 5, 9}, {9, 9}}, 
 
>     makeFiniteWeight[{9, 9}], finiteWeight @@ {Length[{9, 9}], {9, 9}}, 
 
>     {{Length[{9, 9}], 2}, {2, {9, 9}}}, finiteWeight @@ {2, {9, 9}}, 
 
>     finiteWeight[2, {9, 9}]}, 
 
>    finiteWeight[2, {6, 6}] + finiteWeight[2, {9, 9}], 
 
>    makeFiniteWeight[finiteWeight[2, {6, 6}][standardBase] + 
 
>      finiteWeight[2, {9, 9}][standardBase]], 
 
>    {{finiteWeight[2, {6, 6}][standardBase], finiteWeight[2, {6, 6}][[2]], 
 
>      {6, 6}}, {finiteWeight[2, {9, 9}][standardBase], 
 
>      finiteWeight[2, {9, 9}][[2]], {9, 9}}, {6, 6} + {9, 9}, 
 
>     {6 + 9, 6 + 9}, {6 + 9, 15}, {6 + 9, 15}, {15, 15}}, 
 
>    makeFiniteWeight[{15, 15}], 
 
>    finiteWeight @@ {Length[{15, 15}], {15, 15}}, 
 
>    {{Length[{15, 15}], 2}, {2, {15, 15}}}, finiteWeight @@ {2, {15, 15}}, 
 
>    finiteWeight[2, {15, 15}]}

Out[8]= $Aborted

FullForm[Trace[Plus @@ {1,2,3}]]

finiteWeight/:Plus[wgs__finiteWeight]:=
    makeFiniteWeight[Plus @@ (#[standardBase]&/@ {wgs})]


Out[2]//FullForm= 
 
>   List[HoldForm[Apply[Plus, List[1, 2, 3]]], HoldForm[Plus[1, 2, 3]], 
 
>    HoldForm[6]]

Out[1]= {Plus @@ {1, 2, 3}, 1 + 2 + 3, 6}

[Calculating...]

Total[Table[makeFiniteWeight[{i,i}],{i,16}]]//Timing

Out[34]= $Aborted

Out[33]= {1.24008, finiteWeight[2, {78, 78}]}

Out[32]= finiteWeight[2, {78, 78}]

Total[Table[{i,i},{i,200}]]//Timing


pp=Table[{i,i},{i,200}]

Total[pp]//Timing

Plus[1,2,3]


?Unprotect

?Protect

Protect[s , s , ...] sets the attribute Protected for the symbols s
         1   2                                                     i
    . Protect["form ", "form ", ...]
                   1        2
      protects all symbols whose names match any of the string patterns form
                                                                            i
      . 

Unprotect[s , s , ...] removes the attribute Protected for the symbols s
           1   2                                                        i
    . Unprotect["form ", "form ", ...]
                     1        2
      unprotects all symbols whose names textually match any of the form . 
                                                                        i

Unprotect[Plus]

Plus[{wgs__finiteWeight}]:=
    makeFiniteWeight[Plus[#[standardBase]&/@ {wgs}]]

Protect[Plus]

Protect[Plus];


Out[22]= 6

Out[20]= {20100, 20100}

Out[17]= {0., {20100, 20100}}

Total[Table[makeFiniteWeight[{i,i}],{i,12}]]//Timing

Trace[Total[Table[makeFiniteWeight[{i,i}],{i,5}]]]

Out[6]= {{Table[makeFiniteWeight[{i, i}], {i, 5}], 
 
>     {{{i, 1}, {i, 1}, {1, 1}}, makeFiniteWeight[{1, 1}], 
 
>      finiteWeight @@ {Length[{1, 1}], {1, 1}}, 
 
>      {{Length[{1, 1}], 2}, {2, {1, 1}}}, finiteWeight @@ {2, {1, 1}}, 
 
>      finiteWeight[2, {1, 1}]}, 
 
>     {{{i, 2}, {i, 2}, {2, 2}}, makeFiniteWeight[{2, 2}], 
 
>      finiteWeight @@ {Length[{2, 2}], {2, 2}}, 
 
>      {{Length[{2, 2}], 2}, {2, {2, 2}}}, finiteWeight @@ {2, {2, 2}}, 
 
>      finiteWeight[2, {2, 2}]}, 
 
>     {{{i, 3}, {i, 3}, {3, 3}}, makeFiniteWeight[{3, 3}], 
 
>      finiteWeight @@ {Length[{3, 3}], {3, 3}}, 
 
>      {{Length[{3, 3}], 2}, {2, {3, 3}}}, finiteWeight @@ {2, {3, 3}}, 
 
>      finiteWeight[2, {3, 3}]}, 
 
>     {{{i, 4}, {i, 4}, {4, 4}}, makeFiniteWeight[{4, 4}], 
 
>      finiteWeight @@ {Length[{4, 4}], {4, 4}}, 
 
>      {{Length[{4, 4}], 2}, {2, {4, 4}}}, finiteWeight @@ {2, {4, 4}}, 
 
>      finiteWeight[2, {4, 4}]}, 
 
>     {{{i, 5}, {i, 5}, {5, 5}}, makeFiniteWeight[{5, 5}], 
 
>      finiteWeight @@ {Length[{5, 5}], {5, 5}}, 
 
>      {{Length[{5, 5}], 2}, {2, {5, 5}}}, finiteWeight @@ {2, {5, 5}}, 
 
>      finiteWeight[2, {5, 5}]}, 
 
>     {finiteWeight[2, {1, 1}], finiteWeight[2, {2, 2}], 
 
>      finiteWeight[2, {3, 3}], finiteWeight[2, {4, 4}], 
 
>      finiteWeight[2, {5, 5}]}}, 
 
>    Total[{finiteWeight[2, {1, 1}], finiteWeight[2, {2, 2}], 
 
>      finiteWeight[2, {3, 3}], finiteWeight[2, {4, 4}], 
 
>      finiteWeight[2, {5, 5}]}], finiteWeight[2, {15, 15}]}

Out[15]= {{Table[makeFiniteWeight[{i, i}], {i, 5}], 
 
>     {{{i, 1}, {i, 1}, {1, 1}}, makeFiniteWeight[{1, 1}], 
 
>      {NumberQ[1], True}, {NumberQ[1], True}, 
 
>      finiteWeight @@ {Length[{1, 1}], {1, 1}}, 
 
>      {{Length[{1, 1}], 2}, {2, {1, 1}}}, finiteWeight @@ {2, {1, 1}}, 
 
>      finiteWeight[2, {1, 1}]}, 
 
>     {{{i, 2}, {i, 2}, {2, 2}}, makeFiniteWeight[{2, 2}], 
 
>      {NumberQ[2], True}, {NumberQ[2], True}, 
 
>      finiteWeight @@ {Length[{2, 2}], {2, 2}}, 
 
>      {{Length[{2, 2}], 2}, {2, {2, 2}}}, finiteWeight @@ {2, {2, 2}}, 
 
>      finiteWeight[2, {2, 2}]}, 
 
>     {{{i, 3}, {i, 3}, {3, 3}}, makeFiniteWeight[{3, 3}], 
 
>      {NumberQ[3], True}, {NumberQ[3], True}, 
 
>      finiteWeight @@ {Length[{3, 3}], {3, 3}}, 
 
>      {{Length[{3, 3}], 2}, {2, {3, 3}}}, finiteWeight @@ {2, {3, 3}}, 
 
>      finiteWeight[2, {3, 3}]}, 
 
>     {{{i, 4}, {i, 4}, {4, 4}}, makeFiniteWeight[{4, 4}], 
 
>      {NumberQ[4], True}, {NumberQ[4], True}, 
 
>      finiteWeight @@ {Length[{4, 4}], {4, 4}}, 
 
>      {{Length[{4, 4}], 2}, {2, {4, 4}}}, finiteWeight @@ {2, {4, 4}}, 
 
>      finiteWeight[2, {4, 4}]}, 
 
>     {{{i, 5}, {i, 5}, {5, 5}}, makeFiniteWeight[{5, 5}], 
 
>      {NumberQ[5], True}, {NumberQ[5], True}, 
 
>      finiteWeight @@ {Length[{5, 5}], {5, 5}}, 
 
>      {{Length[{5, 5}], 2}, {2, {5, 5}}}, finiteWeight @@ {2, {5, 5}}, 
 
>      finiteWeight[2, {5, 5}]}, 
 
>     {finiteWeight[2, {1, 1}], finiteWeight[2, {2, 2}], 
 
>      finiteWeight[2, {3, 3}], finiteWeight[2, {4, 4}], 
 
>      finiteWeight[2, {5, 5}]}}, 
 
>    Total[{finiteWeight[2, {1, 1}], finiteWeight[2, {2, 2}], 
 
>      finiteWeight[2, {3, 3}], finiteWeight[2, {4, 4}], 
 
>      finiteWeight[2, {5, 5}]}], finiteWeight[2, {15, 15}]}



Out[14]= $Aborted

Out[13]= $Aborted

Out[12]= finiteWeight[2, {55, 55}]

Out[11]= {finiteWeight[2, {1, 1}], finiteWeight[2, {2, 2}], 
 
>    finiteWeight[2, {3, 3}], finiteWeight[2, {4, 4}], 
 
>    finiteWeight[2, {5, 5}], finiteWeight[2, {6, 6}], 
 
>    finiteWeight[2, {7, 7}], finiteWeight[2, {8, 8}], 
 
>    finiteWeight[2, {9, 9}], finiteWeight[2, {10, 10}]}

                         7  5  3  1
Out[8]= finiteWeight[4, {-, -, -, -}]
                         2  2  2  2

                         7  5  3  1
Out[7]= finiteWeight[4, {-, -, -, -}]
                         2  2  2  2

Out[6]= 2772

rho[pr]

Out[7]= $Aborted

b6=makeSimpleRootSystem[B,6]

dimension[b6][weight[b6][1,0,0,0,2,0]]

Out[4]= 2844270

ff=freudenthalMultiplicities[b6][weight[b6][1,0,0,0,2,0]]//Timing

f1

[Calculating...]

Out[26]= {31.43, mults$1366}

Out[25]= mults$1364



Out[24]= 2844270

Out[23]= 188663555808

Out[22]= 13

freudenthalMultiplicities[makeSimpleRootSystem[B,6]]

fe=simpleBranching[makeSimpleRootSystem[B,4],regularSubalgebra[makeSimpleRootSystem[B,4]][3,4]][wg]//Timing

Out[20]= {5.84437, formalElement[table$1330]}

fe=ourBranching[makeSimpleRootSystem[B,4],regularSubalgebra[makeSimpleRootSystem[B,4]][2,3,4]][wg]//Timing

res=freudenthalMultiplicities[b4][wg]//Timing

res2=racahMultiplicities[b4][wg]//Timing

Out[20]= {9.01256, mults$2231}

Length[values[res[[2]]]]

Out[19]= 12

Out[18]= 1

Out[17]= {0.912057, mults$2229}

freudenthalMultiplicities[b4][wg]//Timing

Out[16]= {1.40009, mults$2227}

Out[15]= mults$2225

Out[14]= {5.64835, formalElement[table$2224]}

Out[13]= {8.41653, formalElement[table$1098]}

Out[12]= formalElement[table$1047]

Out[8]= formalElement[table$1046]

Export["br4.png",draw3dProjection[4,2,3,fe]];

dimension[positiveRoots[b4]][weight[b4][1,0,0,0]]


Out[7]= simpleBranching[finiteRootSystem[4, 4, 
 
>     {finiteWeight[4, {1, -1, 0, 0}], finiteWeight[4, {0, 1, -1, 0}], 
 
>      finiteWeight[4, {0, 0, 1, -1}], finiteWeight[4, {0, 0, 0, 1}]}], 
 
>    finiteRootSystem[3, 4, {finiteWeight[4, {0, 1, -1, 0}], 
 
>      finiteWeight[4, {0, 0, 1, -1}], finiteWeight[4, {0, 0, 0, 1}]}], 
 
>    finiteWeight[4, {2, 2, 1, 1}]]

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
	      Export["br1.pdf",drawPlaneProjection[4,3,fe]];
	      fe=simpleBranching[b4,b2][wg];
	      Export["br2.pdf",drawPlaneProjection[4,3,fe]];
	      fe=branching2[b4,b2][wg];
	      Export["br3.pdf",drawPlaneProjection[4,3,fe]];
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

