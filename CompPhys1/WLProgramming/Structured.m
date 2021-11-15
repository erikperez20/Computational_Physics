(* Wolfram Language Package *)

(* Created by the Wolfram Workbench 16 sep. 2021 *)

BeginPackage["Structured`"]
(* Exported symbols added here with SymbolName::usage *) 

sumNaturalsS::usage = "sumNaturalsS calcula la suma de 1 hasta n"
factorialS::usage = "factorialS calcula la productoria de n"
polynomialBaseS::usage = "polynomialBaseS calcula la base polinomial de orden n para la variable x"
reverseSublistsS::usage = "reverseSublistsS invierte el orden las sublistas dado una lista de listas"
removeSequentialRepetitionsS::usage = "removeSequentialRepetitionsS remueve las secuencias repetidas en una lista"
multiplyPolynomialsS::usage = "Multiplica dos polinomios y los expande"
apply::usage = "Funcion apply de forma estructural"
lispInterpreterS1::usage = "lisp interpreter version 1"
lispInterpreterS2::usage = "lisp interpreter version 2"

Begin["`Private`"]
(* Implementation of the package *)

sumNaturalsS[n_Integer?Positive] := Module[{k, sum = 0},
	Do[sum = sum + k, {k, 1, n}]; sum]
	
	(*sumNaturalsS:
		-	procedural programming: iteration
		-	cumulative partials sums*)
	
	
factorialS[n_Integer?Positive] := Module[{k, prod = 1},
  Do[prod = prod*k, {k, n}]; prod]
	
	(*factorialS:
		-	procedural programming: iteration
		-	cumulative partials products*)

polynomialBaseS[n_Integer?Positive, x_Symbol] := Module[{temp, list},
  temp = 1;
  list = {1};(*no mem allocation*)
  Do[AppendTo[list, temp = x*temp], {n}]; list]
	
	(*polynomialBaseS:
		-	procedural programming: iteration
		-	generate a list, one element each loop
		-	no memory allocation*)

(*La condicional verifica que las sublistas tengan profundidad 2, osea sean listas de una dimension*)
reverseSublistsS[lista_List?(!MemberQ[#, False] &@ Map[(Depth[#]==2)&, #] &)] := 
 Module[{i, j, len, temp = lista}(*copy & mem alloc*),
  
  (*nested iteration form*)
  Do[
   len = Length[lista[[i]]];
   Do[
    temp[[i, j]] = lista[[i, len - j + 1]];
    temp[[i, len - j + 1]] = lista[[i, j]],
    {j, Floor[len/2]}], {i, 1, Length[lista]}]; temp]

	(*reverseSublistsS:
		-	procedural programming: nested loop
		-	access to list elements by indexation
		-	memory allocation: make a copy of the list*)

(*La condicional verifica que la lista tenga profundidad 2, sea una lista plana sin sublistas*)
removeSequentialRepetitionsS[lista_List?(Depth[#] == 2 &)] :=
 Module[{newlist = {First[lista]}, elem},
  (*no alloc possible*)
  Do[
   (*Create list on the fly*)
   If[!(elem === Last[newlist]),
    AppendTo[newlist, elem]],
   {elem, Rest[lista]}
   ];
  newlist
  ]

	(*removeSequentialRepetitionsS:
		-	procedural programming: iteration and branching
		-	generate a list, one element each loop
		-	no memory allocation*)

multiplyPolynomialsS[p1_, p2_, x_Symbol] := 
 Module[{product, pol1 = p1, pol2 = p2, i},
  pol1[[0]] = List; (*Cambiar la cabecera del polinomio a una lista*)
  pol2[[0]] = List;
  product = {};
  Do[AppendTo[product, Times[i, j]], {i, pol1}, {j, pol2}];
  Total[product]
  ]

apply[op_Symbol,list_List]:=
	Module[{temp = list},
		temp[[0]] = Sequence;
		op[temp]
	]
	
	(*apply similar to Apply, but procedural*)

lispInterpreterS1::notop = "`1` is not a valid operator.";

(*Condicion de que la lista sea de 3 elementos y de profundidad 2 (lista plana de 1 nivel)*)
lispInterpreterS1[list_List?(Depth[#] == 2 && Length[#] == 3 &)]:=
	Module[ {op = First[list], nums = Rest[list]},
	
		Which[
			op == "+",
				apply[Plus, nums],
			op == "-",
				apply[Subtract, nums],
			op == "*",
				apply[Times, nums],
			op == "/",
				apply[Divide, nums],
			True,
				Message[lispInterpreterS1::notop, op];
				$Failed
			]	
		] 

lispInterpreterS2::notop = "`1` is not a valid operator.";

lispInterpreterS2[list_List?(Depth[#] == 2 && Length[#] == 3 &)]:=
	Module[ {op = First[list], nums = Rest[list]},
	
		Switch[op,
			"+",
				apply[Plus, nums],
			"-",
				apply[Subtract, nums],
			"*",
				apply[Times, nums],
			"/",
				apply[Divide, nums],
			_,
				Message[lispInterpreterS2::notop, op];
				$Failed
			]	
		] 

	(*lispInterpreterS:
		-	procedural programming: multiple branching
		-	version 1: Which, version 2: Switch
		-	uses the auxiliary function apply
		-	error message for incorrect operator*)

(* To be sure that this file is loaded ...*)
Print["Structured definitions loaded."]

End[]

EndPackage[]

