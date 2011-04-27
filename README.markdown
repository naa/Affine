Affine.m
==========

[Mathematica](http://www.wolfram.com/mathematica/) program for computations in representation theory of affine and finite-dimensional Lie algebras.

Overview
----------

This program is based on the properties of weight system and uses Weyl symmetry. Central problems are weight multiplicity calculation, branching of representation to representation of subalgebras and tensor product decomposition. For more detail see `doc/manual.pdf`.

System requirements
----------

Mathematica. Tested with versions 7 and 8 on Linux and Windows.
More details in `doc/manual.pdf`

Installation
----------

`git clone git://github.com/naa/Affine.git`

Add path to the beginning of your notebook
`AppendTo[$Path,"src/"];`

Usage
----------

`<<affine.m;`

Use the functions of the package. 
See `demo/demo.nb` and `doc/manual.pdf`
