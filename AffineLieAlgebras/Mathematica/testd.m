AppendTo[$Path,"/home/anton/study/2011/Articles/AffineLieAlgebras/Mathematica/"];

<<datastructures.m;

?finiteWeight

?==

orbit[b2][{makeFiniteWeight[{2,-1}]}]

ChebyshevU[5,x]

5!

M[p_,k_]/;Mod[p,2]==Mod[k,2]:=p!(k+1)/((p-k)/2)!/((p+k+2)/2)!;
M[p_,k_]/;Mod[p,2]!=Mod[k,2]:=0

Clear[M]

MM:=Table[M[i,j],{i,10},{j,10}]

MM//MatrixForm

Out[53]//MatrixForm= 1   0   0   0   0

                     0   1   0   0   0

                     2   0   1   0   0

                     0   3   0   1   0

                     5   0   4   0   1

Inverse[MM].((2*x)^Range[10])

                  2            3       2       4            3       5
Out[69]= {2 x, 4 x , -4 x + 8 x , -12 x  + 16 x , 6 x - 32 x  + 32 x , 
 
         2       4       6             3        5        7
>    24 x  - 80 x  + 64 x , -8 x + 80 x  - 192 x  + 128 x , 
 
          2        4        6        8
>    -40 x  + 240 x  - 448 x  + 256 x , 
 
                 3        5         7        9
>    10 x - 160 x  + 672 x  - 1024 x  + 512 x , 
 
         2        4         6         8         10
>    60 x  - 560 x  + 1792 x  - 2304 x  + 1024 x  }

                          2            3          2       4
Out[66]= {1, 2 x, -2 + 4 x , -6 x + 8 x , 3 - 16 x  + 16 x }

Range[0,5]

Out[65]= {0, 1, 2, 3, 4, 5}

                  2            3       2       4            3       5
Out[57]= {2 x, 4 x , -4 x + 8 x , -12 x  + 16 x , 6 x - 32 x  + 32 x }

ChebyshevU[#,x]&/@ Range[10]

                       2            3          2       4
Out[70]= {2 x, -1 + 4 x , -4 x + 8 x , 1 - 12 x  + 16 x , 
 
               3       5           2       4       6
>    6 x - 32 x  + 32 x , -1 + 24 x  - 80 x  + 64 x , 
 
                3        5        7          2        4        6        8
>    -8 x + 80 x  - 192 x  + 128 x , 1 - 40 x  + 240 x  - 448 x  + 256 x , 
 
                 3        5         7        9
>    10 x - 160 x  + 672 x  - 1024 x  + 512 x , 
 
              2        4         6         8         10
>    -1 + 60 x  - 560 x  + 1792 x  - 2304 x  + 1024 x  }

                          2            3          2       4
Out[67]= {1, 2 x, -1 + 4 x , -4 x + 8 x , 1 - 12 x  + 16 x }

                       2            3          2       4            3       5
Out[59]= {2 x, -1 + 4 x , -4 x + 8 x , 1 - 12 x  + 16 x , 6 x - 32 x  + 32 x }

Out[58]= {1, 2, 3, 4, 5}

x^Range[10]

              2   3   4   5   6   7   8   9   10
Out[56]= {x, x , x , x , x , x , x , x , x , x  }

Out[55]= {1, 2, 3, 4, 5, 6, 7, 8, 9, 10}

Out[54]//MatrixForm= 1    0    0    0    0

                     0    1    0    0    0

                     -2   0    1    0    0

                     0    -3   0    1    0

                     3    0    -4   0    1

M[3,3]

Simplify[Plus@@Table[M[5,i]*ChebyshevU[i,x],{i,0,5}]]

             5
Out[51]= 32 x

                    3       5                3
Out[50]= 16 x - 32 x  + 32 x  + 4 (-4 x + 8 x )

                                   3                3       5
Out[49]= {0, 10 x, 0, 4 (-4 x + 8 x ), 0, 6 x - 32 x  + 32 x }

                                   3                3       5
Out[48]= {0, 10 x, 0, 4 (-4 x + 8 x ), 0, 6 x - 32 x  + 32 x }

                                   3                3       5
Out[47]= {0, 10 x, 0, 4 (-4 x + 8 x ), 0, 6 x - 32 x  + 32 x }

                                      2
           1024         1024 (-1 + 4 x )               3
Out[44]= {------, 10 x, ----------------, 4 (-4 x + 8 x ), 
          105 Pi             63 Pi
 
                   2       4
     5120 (1 - 12 x  + 16 x )            3       5
>    ------------------------, 6 x - 32 x  + 32 x }
              693 Pi

Out[43]= 1

Out[42]= 2

Out[41]//MatrixForm= 1    0    0    0    0

                     0    1    0    0    0

                     -2   0    1    0    0

                     0    -3   0    1    0

                     3    0    -4   0    1

Out[40]//MatrixForm= 1   0   0   0   0

                     0   1   0   0   0

                     2   0   1   0   0

                     0   3   0   1   0

                     5   0   4   0   1

Out[38]//MatrixForm= 0   0   0   0   0

                     0   1   0   0   0

                     0   0   0   0   0

                     0   3   0   1   0

                     0   0   0   0   0

                                   1
Out[37]= {{0, 0, 0, 0, 0}, {0, ----------, 0, 0, 0}, {0, 0, 0, 0, 0}, 
                               MatrixForm
 
             3              1
>    {0, ----------, 0, ----------, 0}, {0, 0, 0, 0, 0}}
         MatrixForm     MatrixForm

?Mod

?&&

e  && e  && ... is the logical AND function. It evaluates its arguments in
 1     2
    order, giving False immediately if any of them are False, and True if they
    are all True. 

Mod[m, n] gives the remainder on division of m
     by n. Mod[m, n, d] uses an offset d. 


Out[33]= {{15, 0, 0, 0, 0}, {-45, 15, 0, 0, 0}, {90, -75, 15, 0, 0}, 
 
>    {-150, 225, -105, 15, 0}, {225, -525, 420, -135, 15}}

Out[32]= 5 Null

Inverse[MM]//MatrixForm

Out[31]//MatrixForm= 1     0     0     0     0

                     -3    1     0     0     0

                     6     -5    1     0     0

                     -10   15    -7    1     0

                     15    -35   28    -9    1

Out[30]//MatrixForm= 
 
>   MatrixInverse[{{1, 0, 0, 0, 0}, {3, 1, 0, 0, 0}, {9, 5, 1, 0, 0}, 
     
    >    {28, 20, 7, 1, 0}, {90, 75, 35, 9, 1}}]

                                 1
Power::infy: Infinite expression - encountered.
                                 0

                                 1
Power::infy: Infinite expression - encountered.
                                 0

                                 1
Power::infy: Infinite expression - encountered.
                                 0

General::stop: Further output of Power::infy
     will be suppressed during this calculation.

Out[29]//MatrixForm= 
 


>   1                 ComplexInfinity   ComplexInfinity   ComplexInfinity
     


    >       ComplexInfinity

    1
    -
    3                 1                 ComplexInfinity   ComplexInfinity
     


    >       ComplexInfinity

    1                 1
    -                 -
    9                 5                 1                 ComplexInfinity
     


    >       ComplexInfinity

    1                 1                 1
    --                --                -
    28                20                7                 1
     


    >       ComplexInfinity

    1                 1                 1                 1
    --                --                --                -
    90                75                35                9
     


    >       1

Out[28]//MatrixForm= 1    0    0    0    0

                     3    1    0    0    0

                     9    5    1    0    0

                     28   20   7    1    0

                     90   75   35   9    1

Out[27]= {{1, 0, 0, 0, 0}, {3, 1, 0, 0, 0}, {9, 5, 1, 0, 0}, 
 
>    {28, 20, 7, 1, 0}, {90, 75, 35, 9, 1}}

               8        -8         8        -8         8
Out[25]= {{1, ----, 0, -----, 0, -----, 0, -----, 0, ------}, 
              5 Pi     21 Pi     45 Pi     77 Pi     117 Pi
 
       64        128        -64         256        -320
>    {-----, 1, ------, 0, ------, 0, -------, 0, -------, 0}, 
      15 Pi     105 Pi     315 Pi     3465 Pi     9009 Pi
 
          192       64        -64         192         -64
>    {2, -----, 1, -----, 0, ------, 0, -------, 0, -------}, 
         35 Pi     63 Pi     495 Pi     5005 Pi     4095 Pi
 
       1024       2048       1024        -4096         1024
>    {------, 3, ------, 1, -------, 0, --------, 0, --------, 0}, 
      105 Pi     315 Pi     1155 Pi     45045 Pi     45045 Pi
 
         1024       5120       1024        -1024         1024
>    {5, -----, 4, ------, 1, -------, 0, --------, 0, --------}, 
         63 Pi     693 Pi     1287 Pi     15015 Pi     69615 Pi
 
       8192      16384       8192        32768         -8192
>    {------, 9, ------, 5, -------, 1, --------, 0, ---------, 0}, 
      315 Pi     693 Pi     1001 Pi     45045 Pi     153153 Pi
 
           8192        40960       57344        8192         -8192
>    {14, ------, 14, -------, 6, -------, 1, --------, 0, ---------}, 
          165 Pi      1287 Pi     6435 Pi     12155 Pi     188955 Pi
 
      262144       524288       262144       1048576       262144
>    {-------, 28, -------, 20, -------, 7, ---------, 1, ---------, 0}, 
      3465 Pi      6435 Pi      6435 Pi     109395 Pi     415701 Pi
 
          786432       262144       1835008       2359296       262144
>    {42, -------, 48, -------, 27, --------, 8, ---------, 1, ---------}, 
          5005 Pi      2145 Pi      36465 Pi     230945 Pi     440895 Pi
 
      2097152      4194304       2097152        8388608      10485760
>    {-------, 90, --------, 75, --------, 35, ---------, 9, ---------, 1}}
      9009 Pi      15015 Pi      12155 Pi      138567 Pi     969969 Pi

Out[23]= 2

Out[21]= 120

                   3       5
Out[20]= 6 x - 32 x  + 32 x

ChebyshevU[n, x] gives the Chebyshev polynomial of the second kind U (x). 
                                                                    n

ChebyshevDistance ChebyshevT        ChebyshevU

Out[17]= {{finiteWeight[2, {2, 1}]}, 
 
>    {finiteWeight[2, {1, 2}], finiteWeight[2, {2, -1}]}, 
 
>    {finiteWeight[2, {-1, 2}], finiteWeight[2, {1, -2}]}, 
 
>    {finiteWeight[2, {-2, 1}], finiteWeight[2, {-1, -2}]}, 
 
>    {finiteWeight[2, {-2, -1}]}}

partialOrbit[b2][{makeFiniteWeight[{2,-1}]}]

Out[16]= {{finiteWeight[2, {2, -1}]}, {finiteWeight[2, {-1, 2}]}, 
 
>    {finiteWeight[2, {-1, -2}]}, {finiteWeight[2, {-2, -1}]}}

Out[15]= {{finiteWeight[2, {1, 0}]}, {finiteWeight[2, {0, 1}]}, 
 
>    {finiteWeight[2, {0, -1}]}, {finiteWeight[2, {-1, 0}]}}

Out[14]= {{finiteWeight[2, {-1, 0}]}}



Out[11]= partialOrbit[b2][{finiteWeight[2, {-1, 0}]}]

Out[10]= partialOrbit[b2][finiteWeight[2, {-1, 0}]]

lhs == rhs returns True if lhs and rhs
      are identical. 
      It is defined for weights of finite and affine Lie algebras

?Dot

?Plus

x + y + z represents a sum of terms. 
          It is defined for weights of finite and affine Lie algebras

x + y + z represents a sum of terms. 

a . b . c or Dot[a, b, c] gives products of vectors, matrices and tensors. Dot
     product for finite weights

Dot product for finite weightsDot product for finite weights

Dot product for finite weights

a . b . c or Dot[a, b, c] gives products of vectors, matrices and tensors. 

finiteWeight[dimension_?NomberQ,coordinates_standardBase] represents vector in
   weight space of finite-dimensional Lie algebra.
 finiteWeight[dimension] returns dimension of the space, where weight vector
   is embedded (i.e. for sl_n it is n+1.
 finiteWeight[standardBase] returns standard base coordinates of weight of
   finite-dimensional Lie algebra

finiteWeight[dimension_?NomberQ,coordinates_standardBase] represents vector in
   weight space of finite-dimensional Lie algebra. Dimension is saved
   separately for simplification of testing

finiteWeight[2, {-1, -1}]
finiteWeight[2, {-1, 0}]
finiteWeight[2, {0, -1}]
finiteWeight[2, {-1, 1}]
finiteWeight[2, {1, -1}]
finiteWeight[2, {0, 1}]
finiteWeight[2, {1, 1}]
finiteWeight[2, {1, 0}]
                 3  1
finiteWeight[2, {-, -}]
                 2  2
{Null}
                 1  3
finiteWeight[2, {-, -}]
                 2  2
                 3    1
finiteWeight[2, {-, -(-)}]
                 2    2
{Null, Null}
                   1   3
finiteWeight[2, {-(-), -}]
                   2   2
                 1    3
finiteWeight[2, {-, -(-)}]
                 2    2
{Null, Null}
                   3   1
finiteWeight[2, {-(-), -}]
                   2   2
                   1     3
finiteWeight[2, {-(-), -(-)}]
                   2     2
{Null, Null}
                   3     1
finiteWeight[2, {-(-), -(-)}]
                   2     2
{Null}

Map[toFundamentalChamber[makeSimpleRootSystem[B,2]],{makeFiniteWeight[{-1,-1}],makeFiniteWeight[{-2,-1}]}]

Out[4]= {finiteWeight[2, {1, 1}], finiteWeight[2, {2, 1}]}

Expect["We can use this and other functions for mapping",True,
       Map[toFundamentalChamber[makeSimpleRootSystem[B,2]],{makeFiniteWeight[{-1,-1}],makeFiniteWeight[{-2,-1}]}]==
       {makeFiniteWeight[{1, 1}], makeFiniteWeight[{2, 1}]}]

b2=makeSimpleRootSystem[B,2];

orbit[b2][{makeFiniteWeight[{1,0}],makeFiniteWeight[{1,1}]}]

Out[17]= {{finiteWeight[2, {1, 0}], finiteWeight[2, {1, 1}]}, 
 
>    {finiteWeight[2, {0, 1}], finiteWeight[2, {1, -1}]}, 
 
>    {finiteWeight[2, {-1, 1}], finiteWeight[2, {0, -1}]}, 
 
>    {finiteWeight[2, {-1, -1}], finiteWeight[2, {-1, 0}]}}

Out[16]= {{finiteWeight[2, {1, 0}]}, {finiteWeight[2, {0, 1}]}, 
 
>    {finiteWeight[2, {0, -1}]}, {finiteWeight[2, {-1, 0}]}}

Out[15]= {{finiteWeight[2, {1, 0}]}, {finiteWeight[2, {0, 1}]}, 
 
>    {finiteWeight[2, {0, -1}]}, {finiteWeight[2, {-1, 0}]}}

Out[12]= {{finiteWeight[2, {1, 0}]}, {finiteWeight[2, {0, 1}]}, 
 
>    {finiteWeight[2, {0, -1}]}, {finiteWeight[2, {-1, 0}]}}

Out[11]= {{finiteWeight[2, {0, 0}]}}

Out[9]= {{finiteWeight[2, {0, 0}]}, {}}

Out[8]= {{finiteWeight[2, {1, 0}]}, {finiteWeight[2, {0, 1}]}, 
 
>    {finiteWeight[2, {0, -1}]}, {finiteWeight[2, {-1, 0}]}, {}}

Out[7]= {{finiteWeight[2, {2, 1}]}, 
 
>    {finiteWeight[2, {1, 2}], finiteWeight[2, {2, -1}]}, 
 
>    {finiteWeight[2, {-1, 2}], finiteWeight[2, {1, -2}]}, 
 
>    {finiteWeight[2, {-2, 1}], finiteWeight[2, {-1, -2}]}, 
 
>    {finiteWeight[2, {-2, -1}]}, {}}

Out[3]= {toFundamentalChamber[makeFiniteRootSystem[B, 2]][finiteWeight[2, 
 
>      {-1, -1}]], toFundamentalChamber[makeFiniteRootSystem[B, 2]][
 
>     finiteWeight[2, {-2, -1}]]}

finiteWeight[2, {1, -1}]
finiteWeight[2, {0, 1}]
finiteWeight[2, {1, 1}]
finiteWeight[2, {1, 0}]
                 3  1
finiteWeight[2, {-, -}]
                 2  2
{Null}
                 1  3
finiteWeight[2, {-, -}]
                 2  2
                 3    1
finiteWeight[2, {-, -(-)}]
                 2    2
{Null, Null}
                   1   3
finiteWeight[2, {-(-), -}]
                   2   2
                 1    3
finiteWeight[2, {-, -(-)}]
                 2    2
{Null, Null}
                   3   1
finiteWeight[2, {-(-), -}]
                   2   2
                   1     3
finiteWeight[2, {-(-), -(-)}]
                   2     2
{Null, Null}
                   3     1
finiteWeight[2, {-(-), -(-)}]
                   2     2
{Null}
{}

fundamentalWeights[rs_affineRootSystem]:=Map[makeAffineWeight[#[[1]],#[[2]],0]&,
					     Transpose[{Prepend[fundamentalWeights[rs[finiteRootSystem]],
								0*rs[finiteRootSystem][simpleRoot][1]],
							comarks[rs]}]]

rs=b2a


Transpose[{Prepend[fundamentalWeights[rs[finiteRootSystem]],
								0*rs[finiteRootSystem][simpleRoot][1]],
							comarks[rs]}]

                  
Out[41]= {{finiteWeight[2, {0, 0}], finiteWeight[2, {1, 0}], 
 
                       1  1
>     finiteWeight[2, {-, -}]}, {1, 1, 1}}
                       2  2

Prepend[fundamentalWeights[rs[finiteRootSystem]],
								0*rs[finiteRootSystem][simpleRoot][1]]

         
Out[40]= {finiteWeight[2, {0, 0}], finiteWeight[2, {1, 0}], 
 
                      1  1
>    finiteWeight[2, {-, -}]}
                      2  2


(* finiteWeight/:0*y_finiteWeight:=makeFiniteWeight[0*y[standardBase]]; *)

fundamentalWeights[b2a]

Out[49]= {affineWeight[2, finiteWeight[2, {0, 0}], 1, 0], 
 
>    affineWeight[2, finiteWeight[2, {1, 0}], 1, 0], 
 
                                      1  1
>    affineWeight[2, finiteWeight[2, {-, -}], 1, 0]}
                                      2  2

comarks[b2a]

Out[39]= {1, 1, 1}

marks[rs]*Map[#.#/2&,rs[simpleRoots]]

b2a[simpleRoots]

Set::write: Tag Times in {affineWeight[2, finiteWeight[2, {-1, -1}], 0, 1], 
      affineWeight[2, finiteWeight[2, {1, -1}], 0, 0], 
      affineWeight[2, finiteWeight[2, {0, 1}], 0, 0]} Out[302] is Protected.

          1 . 1  1 . 1
Out[31]= {-----, -----, 1 . 1}
            2      2

Set::write: Tag Times in {affineWeight[2, finiteWeight[2, {-1, -1}], 0, 1], 
      affineWeight[2, finiteWeight[2, {1, -1}], 0, 0], 
      affineWeight[2, finiteWeight[2, {0, 1}], 0, 0]} Out[302] is Protected.

          1 . 1  1 . 1
Out[30]= {-----, -----, 1 . 1}
            2      2

Out[29]= {1, 1, 2}

rho[rs_affineRootSystem]:=Plus@@fundamentalWeights[rs]



racahMultiplicities[rs_affineRootSystem][highestWeight_affineWeight,gradelimit_?NumberQ]:=
    Module[{rh=rho[rs],weights,mults,c,insideQ,
	    fan,
	    toFC=toFundamentalChamber[rs]},
	   fan=Map[{rh-#[[1]],#[[2]]}&,Rest[orbitWithEps[rs][rh,gradelimit]]];
	   weights=Sort[ Rest[Flatten[weightSystem[rs][highestWeight,gradelimit]]], #1.rh>#2.rh&];
	   mults[highestWeight]=1;
	   insideQ:=IntegerQ[mults[toFC[#]]]&;
	   Scan[Function[v,
			 mults[v]=
			 Plus@@(fan /. {x_finiteWeight,e_Integer}:> If[insideQ[v+x],-e*mults[toFC[v+x]],0])],
		weights];
	   mults]

weightSystem[b2a][rho[b2a],1]

rh=rho[b2a];
rs=b2a;
highestWeight=2*rh;gradelimit=1;
toFundamentalChamber[rs]/@Sort[ Rest[Flatten[weightSystem[rs][highestWeight,gradelimit]]], #1.rh>#2.rh&]

[Calculating...]

                           
Out[78]= {affineWeight[2, finiteWeight[2, {3, 0}], 6, 0], 
 
>    affineWeight[2, finiteWeight[2, {2, 2}], 6, 0], 
 
>    affineWeight[2, finiteWeight[2, {2, 1}], 6, 0], 
 
>    affineWeight[2, finiteWeight[2, {2, 0}], 6, 0], 
 
>    affineWeight[2, finiteWeight[2, {1, 1}], 6, 0], 
 
>    affineWeight[2, finiteWeight[2, {1, 0}], 6, 0], 
 
>    affineWeight[2, finiteWeight[2, {0, 0}], 6, 0]}


mults[highestWeight]=1;
insideQ:=IntegerQ[mults[toFC[#]]]&;

                           
Out[77]= {affineWeight[2, finiteWeight[2, {3, 0}], 6, 0], 
 
>    affineWeight[2, finiteWeight[2, {2, 2}], 6, 0], 
 
>    affineWeight[2, finiteWeight[2, {2, 1}], 6, 0], 
 
>    affineWeight[2, finiteWeight[2, {2, 0}], 6, 0], 
 
>    affineWeight[2, finiteWeight[2, {1, 1}], 6, 0], 
 
>    affineWeight[2, finiteWeight[2, {1, 0}], 6, 0], 
 
>    affineWeight[2, finiteWeight[2, {0, 0}], 6, 0]}

                           
Out[76]= {affineWeight[2, finiteWeight[2, {3, 1}], 6, 0], 
 
>    affineWeight[2, finiteWeight[2, {3, 0}], 6, 0], 
 
>    affineWeight[2, finiteWeight[2, {2, 2}], 6, 0], 
 
>    affineWeight[2, finiteWeight[2, {2, 1}], 6, 0], 
 
>    affineWeight[2, finiteWeight[2, {2, 0}], 6, 0], 
 
>    affineWeight[2, finiteWeight[2, {1, 1}], 6, 0], 
 
>    affineWeight[2, finiteWeight[2, {1, 0}], 6, 0], 
 
>    affineWeight[2, finiteWeight[2, {0, 0}], 6, 0]}

                           
                                           3  1
Out[75]= {affineWeight[2, finiteWeight[2, {-, -}], 3, 0], 
                                           2  2
 
                                      1  1
>    affineWeight[2, finiteWeight[2, {-, -}], 3, 0]}
                                      2  2

                           
                                           1  1
Out[74]= {affineWeight[2, finiteWeight[2, {-, -}], 3, 0]}
                                           2  2

Out[66]= mults$123

mts:=racahMultiplicities[b2a][rho[b2a],1];

mts[makeAffineWeight[{1/2,1/2},3,0]]


Out[71]= {affineWeight[2, finiteWeight[2, {12, 4}], 0, 0], -1}

                                            3  1
Out[72]= {{affineWeight[2, finiteWeight[2, {-, -}], 3, 0]}, 
                                            2  2
 
                                       1  1
>    {affineWeight[2, finiteWeight[2, {-, -}], 3, 0]}, {}}
                                       2  2

                                           3  1
Out[69]= {affineWeight[2, finiteWeight[2, {-, -}], 3, 0], 
                                           2  2
 
                                      1  1
>    affineWeight[2, finiteWeight[2, {-, -}], 3, 0]}
                                      2  2

[Calculating...]

[Calculating...]

Out[62]= {{affineWeight[2, finiteWeight[2, {1, -1}], 0, 0], -1}, 
 
>    {affineWeight[2, finiteWeight[2, {0, 1}], 0, 0], -1}, 
 
>    {affineWeight[2, finiteWeight[2, {2, -1}], 0, 0], 1}, 
 
>    {affineWeight[2, finiteWeight[2, {1, 2}], 0, 0], 1}, 
 
>    {affineWeight[2, finiteWeight[2, {3, 0}], 0, 0], -1}, 
 
>    {affineWeight[2, finiteWeight[2, {2, 2}], 0, 0], -1}, 
 
>    {affineWeight[2, finiteWeight[2, {3, 1}], 0, 0], 1}}



