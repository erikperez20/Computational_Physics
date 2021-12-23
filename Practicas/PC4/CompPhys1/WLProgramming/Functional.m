(* Wolfram Language Package *)

BeginPackage["Functional`"]
(* Exported symbols added here with SymbolName::usage *)  

sumNaturalsF::usage = "sumNaturalsF calcula la suma de 1 hasta n"
factorialF1::usage = "factorialF1 calcula la productoria de n"
factorialF2::usage = "factorialF2 retorna la lista de los factoriales de 1 hasta n"
polynomialBaseF::usage = "polynomialBaseF calcula la base polinomial de orden n para la variable x"
reverseSublistsF::usage = "reverseSublistsF invierte el orden las sublistas dado una lista de listas"
removeSequentialRepetitionsF::usage = "removeSequentialRepetitionsF remueve las secuencias repetidas en una lista"
multiplyPolynomialsF::usage = "Multiplica dos polinomios y los expande"
lispInterpreterF::usage = "lisp interpreter"
binaryListToByteCodes::usage = "Converts binary number into a list of byte codes"

Begin["`Private`"] (* Begin Private Context *) 

sumNaturalsF[n_Integer?Positive] := Plus @@ NestList[# + 1 &, 1, n - 1] (*Genera una lista de 1 a n con NestList y aplica Suma*)

	(* sumNaturalsF
		-	functional programming
		-	Genera una lista de 1 hasta n con neslist y aplica Plus
		-	no local symbols needed
		*)

factorialF1[n_Integer?Positive] := Times @@ NestList[# + 1 &, 1, n - 1] (*Genera una lista de 1 a n con NestList y aplica Producto*)

factorialF2[n_Integer?Positive] := (Times @@ NestList[# + 1 &, 1, # - 1]) & /@ NestList[# + 1 &, 1, n - 1] 

	(* factorialF
		-	functional programming
		-	Version 1: Aplica times a un array generado con nestlist
		-	Version 2: Genera una lista de 1 a n con NestList y aplica la productoria de cada elemento con Map
		-	no local symbols needed
		*)

polynomialBaseF[n_Integer?Positive, x_Symbol] := NestList[x # &, 1, n] 

	(* polynomialBaseF
		-	functional programming
		-	NestList de los productos de x n veces
		-	no local symbols needed
	*)

(*La condicional verifica que las sublistas tengan profundidad 2, osea sean listas de una dimension*)
reverseSublistsF[lista_List?(!MemberQ[#, False] &@ Map[(Depth[#]==2)&, #] &)] := 
				Reverse /@ lista (*Aplica Reverse a la lista con Map*)

(*La condicional verifica que la lista tenga profundidad 2, sea una lista plana sin sublistas*)
removeSequentialRepetitionsF[lista_List?(Depth[#] == 2 &)] := First /@ Split[lista]
	(*removeSequentialRepetitionsF 
		-	functional programming
		-	Particiona la lista por elementos repetidos
		-	Toma el primer elemento de cada sublista de elementos repetidos
		*)

multiplyPolynomialsF[p1_, p2_, x_Symbol] := Outer[Times, p1, p2] (*Aplica Outer con operador Times para multiplicar ambos polinomios*)

parser["+"] = Plus
parser["-"] = Subtract
parser["*"] = Times
parser["/"] = Divide

(* parser function to parse operators*)
(*Condicion de que la lista sea de 3 elementos y de profundidad 2 (lista plana de 1 nivel)*)
lispInterpreterF[list_List?(Depth[#] == 2 && Length[#] == 3 &)] := Apply[parser[First@#], Rest@#] & @ list

	(* lispInterpreterF
		-	functional programming
		-	branching using auxiliary private function
		-	partition the list using pure function
		*)

baseconvert[n_Integer?Positive,b_Integer?Positive]:=
	Fold[#1 b + #2 &, 0, IntegerDigits[n]]		

binaryListToByteCodes[bits_List?(Length[#] == 8 && Depth[#] == 2 && ContainsOnly[#, {0, 1}] &)]:=
	Module[
	
		{nbits = Length[bits], paddedbits, bytecodes},
		
		paddedbits = Join[bits, If[
									Mod[nbits, 8] == 0, 
										{}, 
										Table[0, {8 - Mod[nbits, 8]}]
										]];
		bytecodes = Flatten[(baseconvert[#1, 2] &) /@ Partition[paddedbits, 8]];
		
		{nbits, bytecodes}
	]
	
	(* binaryListToByteCodes
		-	functional programming
		-	using auxiliary functions
		-	modularization
	*)


Print["Functional definitions loaded."]

End[] (* End Private Context *)

EndPackage[]