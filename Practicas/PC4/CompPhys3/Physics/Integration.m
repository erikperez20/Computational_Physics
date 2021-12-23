(* Wolfram Language Package *)

BeginPackage["Integration`"]
(* Exported symbols added here with SymbolName::usage *)  

riemannIntegrate::usage = "riemannIntegrate[f,a,b,n] integrates f from a to b using Riemann method."
booleIntegrate::usage = "Computes an integral using Boole's integration rule"
extendedTrapezoidalIntegrate::usage = "Computes an integral using the extended trapezoidal rule."
extendedSimpsonIntegrate::usage = "Computes an integral using extended Simpson rule"
trapezoidalIntegrate::usage = "integrates f from a to b using mid point strategy up to convergence with trapezoidal rule"

Begin["`Private`"] (* Begin Private Context *) 

(* ============
	Riemann integration method
	* only midpoint rule is implemented
*)

Options[riemannIntegrate] = {Method -> "Midpoint"}

riemannIntegrate[f_, a_, b_, n_, OptionsPattern[]] := 
	Module[{h, k, method, sum = 0.},
		
		method = OptionValue[Method];
		h = N[(b - a)/n];
		
		Switch[method,
			"Midpoint",
				Do[sum = sum + f[a + 0.5*h + k*h], {k, 0, n - 1}];
					h*sum,
				_, Echo["NotImplemented"]
		]
	]

(* ==============
	booleIntegrate
	* Computes an integral using Boole's integration rule
	* "Exact" for 4-order polynomials
*)

booleIntegrate[f_, a_, b_]:=
	Module[
		{h = (b - a)/4.},
		
		(2.*h)/45. {7, 32, 12, 32, 7}  .  Array[f[a + #*h]&, 5, 0]
	]

(* ============
	extendedTrapezoidalIntegrate
	*	Computes an integral using the extended trapezoidal rule
*)

extendedTrapezoidalIntegrate[f_, a_, b_, n_]:=
	Module[
		{h, sum, i},
		
		h = N[(b - a)/n];
		sum = 0.5*(f[a] + f[b]);
		
		Do[
			sum = sum + f[a + i*h],
			{i, 1, n - 1}
		];
		h*sum
	]

(* =============
	extendedSimpsonIntegrate
	*	Computes an integral using extended Simpson rule
	*	Implementation: use the relation with the trapezoidal rule
	*	Number of intervals should be even
*)

extendedSimpsonIntegrate[f_, a_, b_, n_?EvenQ] := 
	1./3. (4. extendedTrapezoidalIntegrate[f, a, b, n] - extendedTrapezoidalIntegrate[f, a, b, n/2]);

(* ===============
	trapezoidalIntegrate
*)

(* Funcion auxiliar para determinar la sumatoria de fi en el algoritmo trapezoidal*)
fterm[f_, a_, h_, n_] := 
	Module[{xvals,fvals,i},
		xvals = Table[a + i * h / 2^(n-1), {i, 1, 2^(n-1), 2}]; (*Coordenadas impares de 2^n-1 divisiones entre a y b*)
		fvals = f/@xvals; (*evaluamos la funcion en cada punto determinado*)
		Total@fvals (*sumamos*)
	]
	
Options[trapezoidalIntegrate] = {
	"PrecisionGoal" -> Automatic,
	"AccuracyGoal" -> Automatic,
	"MaxIterations" -> 20
}

trapezoidalIntegrate[f_, a_, b_, opts:OptionsPattern[]] := 
	Module[{prec, acc, maxiter, h, sOld, sNew, i = 2},
		
		prec = OptionValue["PrecisionGoal"] /. {Automatic -> 16, MachinePrecision -> 16};
		acc = OptionValue["AccuracyGoal"] /. {Automatic -> Infinity, MachinePrecision -> 16};
		maxiter = OptionValue["MaxIterations"];
		
		h = b - a; (*h inicial*)
		sOld = (f[b] + f[a])*h/2.; (*Area inicial*)
		sNew = sOld/2. + h/2.*(f[a + h/2.]); (*Area a segundo orden*)
		
		While[(i < maxiter) && (Abs[sNew - sOld] > Abs[sNew] * 10.^(-prec) + 10.^(-acc)),
			
			i++;
			sOld = sNew;
			sNew = sOld/2. + h/2.^(i - 1) * fterm[f, a, h, i];
		];
		sNew
	]




Print["Integration definitions loaded."]

End[] (* End Private Context *)

EndPackage[]