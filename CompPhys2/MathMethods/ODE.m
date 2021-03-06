(* Wolfram Language Package *)

BeginPackage["ODE`"]
(* Exported symbols added here with SymbolName::usage *)  

eulerNDSolve::usage = "Solves 1 or more 1st order diff equation using euler method"
RK4NDSolve::usage = "Solves 1 or more 1st order diff equation using runge kutta order 4 method"

Begin["`Private`"] (* Begin Private Context *) 

(* Case of only one first-order differential equation*)

eulerNDSolve[f_, yo_?NumericQ, {a_?NumericQ, b_?NumericQ, n_Integer}] :=
	Module[{h, x, y, i},
	
		h = N@((b - a)/(n - 1));
		x = y = Table[0., {n}];
		y[[1]] = yo;
		
		Do[
			x[[i + 1]] = x[[i]] + h;
			y[[i + 1]] = y[[i]] + h f[x[[i]], y[[i]]]
			,
			{i, 1, n-1}
		];
		{x, y}
	] /; !MatchQ[yo, _List]

(* Overloading for the case of several first-order differential equations *)

eulerNDSolve[f_, yo_?(VectorQ[#, NumericQ]&), {a_?NumericQ, b_?NumericQ, n_Integer}] :=
	Module[{h, x, y, m, i},
		
		h = N@((b - a)/(n - 1));
		m = Length[yo];
		x = Table[0., {n}];
		y = Table[0., {m}, {n}];
		y[[All, 1]] = yo;

		Do[
			x[[i + 1]] = x[[i]] + h;
			y[[All, i + 1]] = y[[All, i]] + h f[x[[i]], y[[All, i]]]
			,
			{i, 1, n - 1}
		];
		{x, y}
	]
	
RK4NDSolve[f_, yo_?NumericQ, {a_?NumericQ, b_?NumericQ, n_Integer}] := 
	Module[{h, x, y, i, k1, k2, k3, k4},
	
		h = N@((b - a)/(n - 1));
		x = y = Table[0., {n}];
		y[[1]] = yo;
		
		Do[
			x[[i + 1]] = x[[i]] + h;
			
			k1 = f[x[[i]], y[[i]]];
			k2 = f[x[[i]] + h/2., y[[i]] + k1*h/2.];
			k3 = f[x[[i]] + h/2., y[[i]] + k2*h/2.];
			k4 = f[x[[i]] + h, y[[i]] + k3*h];
			
			y[[i + 1]] = y[[i]] + (1./6.)*h*(k1 + 2.*k2 + 2.*k3 + k4)
			,
			{i, 1, n-1}
		];
		{x, y}
	] /; !MatchQ[yo, _List]

(* Overloading for the case of several first-order differential equations *)

RK4NDSolve[f_, yo_?(VectorQ[#, NumericQ]&), {a_?NumericQ, b_?NumericQ, n_Integer}] :=
	Module[{h, x, y, m, i, k1, k2, k3, k4},
		
		h = N@((b - a)/(n - 1));
		m = Length[yo];
		x = Table[0., {n}];
		y = Table[0., {m}, {n}];
		y[[All, 1]] = yo;

		Do[
			x[[i + 1]] = x[[i]] + h;
			
			k1 = f[x[[i]], y[[All, i]]];
			k2 = f[x[[i]] + h/2., y[[All, i]] + k1*h/2.];
			k3 = f[x[[i]] + h/2., y[[All, i]] + k2*h/2.];
			k4 = f[x[[i]] + h   , y[[All, i]] + k3*h];
			
			y[[All, i + 1]] = y[[All, i]] + (1./6.)*h*(k1 + 2.*k2 + 2.*k3 + k4)
			,
			{i, 1, n - 1}
		];
		{x, y}
	]

Print["ODE definitions loaded."]

End[] (* End Private Context *)

EndPackage[]