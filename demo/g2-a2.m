AppendTo[$Path,$InitialDirectory <> "/../src/"];
<<affine.m;


g2a = makeAffineExtension[makeSimpleRootSystem[G,2]];
g2=makeSimpleRootSystem[G,2];
rts=Extract[Select[positiveRoots[g2],#.#==2&],{{1},{2}}];
a2a = makeAffineExtension[makeFiniteRootSystem[rts]];
a2a[gradeLimit]=5;
g2a[gradeLimit]=5;
hw=weight[g2a][0,1,0];
rtv1=simpleBranching[makeIrreducibleModule[g2a][hw],a2a];
zg=Select[rtv1[weights],grade[#]==0&];
{dynkinLabels[a2a][#],conformalWeight[g2a][hw]-conformalWeight[a2a][#],Affine`Private`stringSelector[rtv1,#,15]}&/@zg

                                                                                 
                      2               2      3      4      5
Out[20]= {{{1, 0, 0}, -, 1 + 2 q + 2 q  + 4 q  + 5 q  + 8 q }, 
                      5
 
                 1              2      3      4      5
>    {{0, 1, 0}, --, 1 + q + 2 q  + 3 q  + 5 q  + 7 q }, 
                 15
 
                 1              2      3      4      5
>    {{0, 0, 1}, --, 1 + q + 2 q  + 3 q  + 5 q  + 7 q }}
                 15


a2a=makeAffineExtension[makeSimpleRootSystem[A,2]];
a2a[gradeLimit]=5;
hw1=weight[a2a][0,1,0];
hw2=weight[a2a][0,1,0];
rtv1=branching[{makeIrreducibleModule[a2a][hw1],makeIrreducibleModule[a2a][hw2]},a2a,Plus];
zg=Select[rtv1[weights],grade[#]==0&];
{dynkinLabels[a2a][#],conformalWeight[a2a][hw1]+conformalWeight[a2a][hw2]-conformalWeight[a2a][#],Affine`Private`stringSelector[rtv1,#,15]}&/@zg

                                                      
                              2      3      4      5      6
Out[21]= {{{0, 2, 0}, 0, 1 + q  + 2 q  + 3 q  + 4 q  + 5 q }, 
 
                 2               2      3      4      5       6
>    {{1, 0, 1}, -, 1 + 2 q + 2 q  + 4 q  + 5 q  + 8 q  + 10 q }}
                 5


a2a=makeAffineExtension[makeSimpleRootSystem[A,2]];
a2a[gradeLimit]=5;
hw1=weight[a2a][1,0,0];
hw2=weight[a2a][0,1,0];
rtv1=branching[{makeIrreducibleModule[a2a][hw1],makeIrreducibleModule[a2a][hw2]},a2a,Plus];
zg=Select[rtv1[weights],grade[#]==0&];
{dynkinLabels[a2a][#],conformalWeight[a2a][hw1]+conformalWeight[a2a][hw2]-conformalWeight[a2a][#],Affine`Private`stringSelector[rtv1,#,15]}&/@zg

                                                      
                      1              2      3      4      5       6
Out[22]= {{{1, 1, 0}, --, 1 + q + 2 q  + 3 q  + 5 q  + 7 q  + 11 q }}
                      15


rtv2=decomposition[makeIrreducibleModule[a2a][hw1],makeIrreducibleModule[a2a][hw2]];

zg=Select[rtv2[weights],grade[#]==0&];
{dynkinLabels[a2a][#],conformalWeight[a2a][hw1]+conformalWeight[a2a][hw2]-conformalWeight[a2a][#],Affine`Private`stringSelector[rtv2,#,15]}&/@zg

         
                      1              2      3      4      5       6
Out[24]= {{{1, 1, 0}, --, 1 + q + 2 q  + 3 q  + 5 q  + 7 q  + 11 q }}
                      15
