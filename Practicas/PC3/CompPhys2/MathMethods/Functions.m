(* Wolfram Language Package *)

BeginPackage["Functions`"]
(* Exported symbols added here with SymbolName::usage *)  

legendreCoefficientesR::usage = "Funcion que determina la lista de coeficientes de los polinomios de legendre Pn(x) de manera recursiva"
legendreCoefficientesS::usage = "Funcion que determina la lista de coeficientes de los polinomios de legendre Pn(x) de manera estructurada"
globaltransformF::usage = "Funcion definida utilizando reglas de transformacion globales"
localtransformF::usage = "Funcion definida utilizando reglas de transformacion local"
structuredF::usage = "Funcion definida utilizando un algoritmo estructurado"
piecewiseF::usage = "Funcion definida usando el algoritmo Piecewise"

Begin["`Private`"] (* Begin Private Context *) 


legendreCoefficientesR[0] = {1}; (*El primer polinomio P0(x) = 1*)
legendreCoefficientesR[1] = {0, 1}; (*El segundo polinomio P1(x) = x*)

legendreCoefficientesR[n_Integer?(# >= 0 &)] := 
	(legendreCoefficientesR[n] =  (2 n - 1)/n * PadLeft[legendreCoefficientesR[n - 1], Length[legendreCoefficientesR[n - 1]] + 1] 
								- (n - 1)/n * PadRight[legendreCoefficientesR[n - 2], Length[legendreCoefficientesR[n - 2]] + 2])

	(* legendreCoefficientesR
		-	Recursive function
		-	Uses memoization
		-	Expands arrays with PadRight and PadLeft *)

legendreCoefficientesS[n_Integer?(# >= 0 &)] := 
		Module[{listpoly ,i}, 
				
				If[n == 0, listpoly = {{1}}];  (*El primer polinomio P0(x) = 1*)
				If[n == 1, listpoly = {{1},{0,1}}]; (*El segundo polinomio P1(x) = x*)
				
				If[ n >= 2,
				listpoly = Table[{}, n + 1];(*listpoly es una lista de longitud n+1 de listas vacias 
											en donde guardaremos los polinomios de 0 hasta n*)
				listpoly[[1]] = {1}; (*El primer polinomio P0(x) = 1*)
				listpoly[[2]] = {0, 1}; (*El segundo polinomio P1(x) = x*)
				
				(*Iteramos de 2 hasta n y calculamos los polinomios iterativamente hasta n usando la formula recursiva de coeficientes*)
				Do[listpoly[[i + 1]] = (2*i - 1)/i PadLeft[listpoly[[i]], Length[listpoly[[i]]] + 1] 
										- (i - 1)/i PadRight[listpoly[[i - 1]], Length[listpoly[[i - 1]]] + 2], {i, 2, n}]];
				
				listpoly[[n+1]] (*Retornamos el ultimo polinomio calculado*)
				]
	(*	legendreCoefficientesS
		-	Structured program
		-	allocates memory
		*)

globaltransformF[x_?(NumericQ[#] && ! MatchQ[#, _Complex]&) /; (x <= 0 || x > 4)] = 0;
globaltransformF[x_?(NumericQ[#] && ! MatchQ[#, _Complex]&) /; (0 < x <= 1)] := x^3/6
globaltransformF[x_?(NumericQ[#] && ! MatchQ[#, _Complex]&) /; (1 < x <= 2)] := 2/3 - 1/2*(x - 2)^2*x
globaltransformF[x_?(NumericQ[#] && ! MatchQ[#, _Complex]&) /; (2 < x <= 3)] := -22/3 + 1/2*x*(20 + (x - 8)*x)
globaltransformF[x_?(NumericQ[#] && ! MatchQ[#, _Complex]&) /; (3 < x <= 4)] := -(1/6)*(x - 4)^3
	(* globaltransformF
		-	Ruled based program
		-	Global transformations (Overloading)
		*)

localtransformF[x_?(NumericQ[#] && ! MatchQ[#, _Complex]&)] := 
		Module[{solution},
 			solution = x /. {var_ /; var <= 0 || var > 4 :> 0,
							var_ /; 0 < var <= 1 :> var^3/6,
							var_ /; 1 < var <= 2 :> 2/3 - 1/2*(var - 2)^2*var,
							var_ /; 2 < var <= 3 :> -22/3 + 1/2*var*(20 + (var - 8)*var),
							var_ /; 3 < var <= 4 :> -(1/6)*(var - 4)^3 };
			solution]

	(* localtransformF
		-	Ruled based program
		-	Local transformations
		*)

structuredF[x_?((NumericQ[#] && !MatchQ[#, _Complex])&)] := 
		Module[{},
			Which[		x <= 0 , 0, 
					0 < x <= 1 , x^3/6,
					1 < x <= 2 , 2/3 - 1/2 * (x - 2)^2 * x,
					2 < x <= 3 , -22/3 + 1/2 * x * (20 + (x - 8)*x),
					3 < x <= 4 , -(1/6) * (x - 4)^3,
					4 < x      , 0]
				]
	(* structuredF
		-	Structured program
		-	Uses Which
		*)

piecewiseF[x_?((NumericQ[#] && !MatchQ[#, _Complex])&)] := 
		Piecewise[{	{0,	x <= 0}, 
					{x^3/6, 0 < x <= 1},
					{2/3 - 1/2 (x - 2)^2 x, 1 < x <= 2}, 
					{-22/3 + 1/2 x (20 + (x - 8) x), 2 < x <= 3}, 
					{-(1/6) (x - 4)^3, 3 < x <= 4}, 
					{0, 4 < x}}]
	
	(* piecewiseF
		-	Uses Mathematica intrinsic function Piecewise
		*)



Print["Functions definitions loaded."]

End[] (* End Private Context *)

EndPackage[]