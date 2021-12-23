(* Wolfram Language Package *)

BeginPackage["FindRoots`"]
(* Exported symbols added here with SymbolName::usage *)  

quadraticSolve::usage = "quadraticSolve[a, b, c] solves the quadratic equation for numerical reals a, b, c"
cardano::usage = "cardano[c1,c2,c3,c4] solves the cubic equation for numerical reals c1, c2, c3 and c4 using the Cardano method"
newtonFindRoot::usage = "newtonFindRoot[f,init] find root of f using Newton's method starting at the initial point init"

Begin["`Private`"] (* Begin Private Context *) 

(* =============================

	Solution of the quadratic equation

*)

quadraticSolve::allsol = "this equation doesn't have a unique solution";
quadraticSolve::nosol = " this equation doesn't have any solutions";
quadraticSolve::complex = "this equation has complex solutions";
quadraticSolve::lineq = "this is a linear equation";

quadraticSolve[a_, b_, c_] := 
	Module[{x1, x2, delta},
		If[a == 0.,
			If[b == 0.,
				If[c == 0.,
					Message[quadraticSolve::allsol];
					Return[{}]
				];
				Message[quadraticSolve::nosol];
				Return[{}]
				,
				Message[quadraticSolve::lineq];
				Return[{N[-c/b]}]
			]
			,
			delta = b*b - 4.*a*c;
			If[delta < 0.,
				Message[quadraticSolve::complex];
				x1 = (-b + I Sqrt[-delta]) / (2.*a);
				x2 = (-b - I Sqrt[-delta]) / (2.*a);
				Return[{x1,x2}];
			];
			delta = Sqrt[delta];
			Which[
				Abs[delta] <= Sqrt[$MachineEpsilon],
					x1 = -b/(2.*a);
					x2 = x1,
				b < 0.,
					x1 = (-b + delta) / (2.*a);
					x2 = 2.*c / (-b + delta),
				b > 0.,
					x1 = (-b - delta) / (2.*a);
					x2 = 2.*c / (-b - delta),
				True,
					x1 = N @ Sqrt[-c/a];
					x2 = -x1;		
			];
			{x1,x2}
		]
	] /; Element[a,Reals] && Element[b, Reals] && Element[c, Reals]

cardano::quadraticEq = "this is a quadratic equation";

cardano[c1_, c2_, c3_, c4_] := 
	Module[{a1, a2, a3, q, r, s1, s2, x1, x2, x3},
		If[c4 == 0. && c3 != 0.,
			
			Message[cardano::quadraticEq];
			Return[quadraticSolve[c3,c2,c1]]
		];
		If[c4 == 0.,
			 
			Return[quadraticSolve[c3,c2,c1]]
			,
			a1 = N[c1 / c4];
			a2 = N[c2 / c4];
			a3 = N[c3 / c4];
			
			q = a2/3. - a3*a3/9.;
			r = 1/6.*(a2 * a3 - 3. * a1) - a3^3/27.;
			
				Which[
					(*Condition 1:*)
					q^3 + r^2 < 0, (*We obtain 3 real roots with this condition*)
					
					s1 = Exp[Log[r + I Sqrt[-(q^3 + r^2)]]/3.]; (*Formula to obtain the principal complex value*)
					s2 = Exp[Log[r - I Sqrt[-(q^3 + r^2)]]/3.]; (*Complex conjugate of s1*)
					
					x1 = -a3/3. + 2.*Re[s1]; (*First real root*)
					x2 = -Re[s1] - a3/3. + 1./2. * Sqrt[3.] * Re[(s1 - s2) * I]; (*2nd real root*)
					x3 = -Re[s1] - a3/3. - 1./2. * Sqrt[3.] * Re[(s1 - s2) * I]; (*3rd real root*)
					
					{x1, x2, x3},
					
					
					(*Condition 2: *)
					q^3 + r^2 == 0, (*We obtaion 2 equal solutions*)
					
					s1 = Surd[r, 3]; (*In this case s1 = s2*)
					
					x1 = -a3/3. + 2.*s1;
					x2 = -s1 - a3/3.; (*x2 and x3 are the same*)
					
					{x1, x2, x2},
					
					
					(*Condition 3: *)
					q^3 + r^2 > 0, (*1 real solution and 2 complex solutions*)
					
					s1 = Surd[r + Sqrt[q^3 + r^2], 3]; (*Real cube roots*)
					s2 = Surd[r - Sqrt[q^3 + r^2], 3];
					
					x1 = -a3/3. + s1 + s2; (*first root*)
					x2 = -(s1 + s2)/2. - a3/3. + 1./2. * I * Sqrt[3.] * (s1 - s2); (*2nd real root*)
					x3 = -(s1 + s2)/2. - a3/3. - 1./2. * I * Sqrt[3.] * (s1 - s2); (*3rd real root*)
					
					{x1, x2, x3}
				]
		]
	] /; Element[c1,Reals] && Element[c2, Reals] && Element[c3, Reals] && Element[c4, Reals]

(* ==========
	Newton's Method
*)

newtonFindRoot::maxit = "Max number of steps `1` reached. Result could be inaccurate.";

Options[newtonFindRoot] = {
	PrecisionGoal -> Automatic,
	MaxIterations -> 20,
	StepMonitor -> None
	}

(*Implementar accuracy goal*)

newtonFindRoot[f_, init_, opts:OptionsPattern[]] :=
	With[{df = f'},
		Module[{k = 1, xi = N@init, relError, maxiter, dx, monitor},
		
			relError = OptionValue[PrecisionGoal] /. Automatic -> 10.^-10;
			maxiter = OptionValue[MaxIterations];
			monitor = OptionValue[StepMonitor];
			dx = Infinity;
			
			If[ monitor =!= None,
				While[Abs[dx] > Abs[xi] relError && k < maxiter,
					k++;
					dx = f[xi]/df[xi];
					xi = xi - dx;
					monitor[xi];				
				]
				,
				While[ Abs[dx] > Abs[xi] relError && k < maxiter,
					k++;
					dx = f[xi]/df[xi];
					xi = xi - dx;					
				];	
			];
			
			If[k == maxiter, Message[newtonFindRoot::maxit, maxiter]];
			xi
		]
	]


Print["FindRoots definitions loaded."]

End[] (* End Private Context *)

EndPackage[]