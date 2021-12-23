(* Wolfram Language Package *)

BeginPackage["NumericalFunctions`"]
(* Exported symbols added here with SymbolName::usage *)  

\[ScriptCapitalG]::usage = "\[ScriptCapitalG] represents the acceleration of gravity"
binomialCoefficient::usage = "binomialCoefficient[n, k] computes the binomial coefficient n in k"
ramanujanPi::usage = "ramanujanPi[n] computes using n terms of the Ramanujan series"
heronSqrt::usage = "heronSqrt[x] computes the sqrt of x using Heron's algorithm"
exp::usage = "exp[x] computes the exponential function using Taylor series"
struveH::usage = "struveHV[v, z] computes the struve function"

Begin["`Private`"] (* Begin Private Context *) 

(* ==============
	Acceleration of gravity
	*	New physical constant: quantity
	*	Give all available precision (up to ~16 digits)
*)

SetAttributes[\[ScriptCapitalG], Constant];
N[\[ScriptCapitalG]] = Quantity[9.8, "Meters"/"Seconds"^2]


(* ==============
	Binomial Coefficient
	*	Overloading:
		-	Numerical machine arguments
		-	Integer arguments
		-	Real arguments
*)

SetAttributes[binomialCoefficient, NHoldRest]

binomialCoefficient[n_, m_Integer] := 
	Module[{i}, 
		Echo["definition 1"];
		NProduct[(n - i + 1.) / i, {i, 1, m}]] /; MachineNumberQ[n]

binomialCoefficient[n_Integer, m_Integer] :=
	(Echo["definition 2"]; Factorial[n] / (Factorial[m] * Factorial[n - m])) 

FunctionExpand[binomialCoefficient[n_, m_]] ^:= 
	(Echo["definition 3"]; Gamma[n + 1] / (Gamma[m + 1] * Gamma[n - m + 1]) /;
		Element[n, Reals] && Element[m, Reals])

(* ================
	Compute Pi using a Ramanujan famous formula
	*	Naive approach: sum the nmax first terms
	*	No convergence check: how many c.f. are correct?
	*	Normally PrecisionGoal and AccuracyGoal should be less than WorkingPrecision

*)

ramanujanPi[nmax_Integer?Positive, wp_Integer: MachinePrecision] :=
	Module[{sum, k}, 
		
		(* Numerical sum setting a WorkingPrecision *)
		sum = NSum[((26390*k + 1103)*(4*k)!)/(396^(4*k)*k!^4), {k, 0, nmax},
				WorkingPrecision -> wp, PrecisionGoal -> wp, AccuracyGoal -> wp];
				
		(* cast to sum precision *)
		9801/(2*Sqrt[2]*sum)		
	]

(* ================
	Heron's algorithm to compute the sqrt of a number
	*	Check convergence using the relative error of succesive approximations
	*	Normally PrecisionGoal and AccuracyGoal should be less than WorkingPrecision
		In this case it works because Heron's Sqrt converges fast enough
*)

Options[heronSqrt] = {
	PrecisionGoal -> Automatic,
	AccuracyGoal -> Automatic};
	
heronSqrt[x_, ini_:1., opts:OptionsPattern[]] :=
	Module[{old, new, prec, acc},
		
		If[ x == 0., Return[0.]];
		
		prec = OptionValue[PrecisionGoal] /. {Automatic -> 16, MachinePrecision -> 16};
		acc = OptionValue[AccuracyGoal] /. {Automatic -> Infinity, MachinePrecision -> 16};
		
		old = 0.;
		new = N[ini];
		
		While[Abs[new - old] > 10.^(-prec) * Abs[new] + 10.^(-acc),
			
			(* calculation is done in MachinePrecision *)
			old = new;
			new  = 0.5*(new + x/new);
			Echo[InputForm[new]]
		];
		
		new
	] /; NumericQ[x] && NumericQ[ini]

(* ======================
	Compute the exp function using Taylor series
	*	exp(-x) behaves bad numerically then use 1/exp(x)
	*	Normally PrecisionGoal and AccuracyGoal should be less than WorkingPrecision
*)

Options[exp] = {
	PrecisionGoal -> Automatic,
	AccuracyGoal -> Automatic
};

exp[x_, opts:OptionsPattern[]] :=
	Module[{n, oldsum, newsum, term, x2, prec, acc},
		
		If[x == 0., Return[1.0]];
		
		If[x < 0., x2 = -x, x2 = x];
		
		prec = OptionValue[PrecisionGoal] /. {Automatic -> 16, MachinePrecision -> 16};
		acc = OptionValue[AccuracyGoal] /. {Automatic -> Infinity, MachinePrecision -> 16};
		
		n = 1;
		oldsum = -1.;
		newsum = 0.;
		term = 1.;
		
		While[ term > newsum * 10.^-prec + + 10.^(-acc),
			oldsum = newsum;
			newsum = newsum + term;
			term = term * x2/n;
			n = n + 1;
		];
		
		If[x < 0., 1./newsum, newsum]
	
	] /; NumericQ[x]

Options[struveH] = {
	PrecisionGoal -> Automatic,
	AccuracyGoal -> Automatic
};

struveH[v_, z_, opts:OptionsPattern[]] := 
	Module[{prec, acc, m, k, newsum, oldsum, term},
		
		Which[
		
		z == 0., 0., (*First Condition*)
		
		OddQ[2*v] || (FractionalPart[v*2] == 0. && OddQ[IntegerPart[v*2]]), 
			Which[ (*Second Condition: v is a rational in the form of ...,-3/2 ,-1/2 ,1/2 ,3/2 ,5/2 ,... - Numerator is Odd *)
					
				v > 0, (*v > 0*)

				term = N[Gamma[1./2.]] * (z/2.)^(v-1) / N[Gamma[v + 1./2.]]; (*first term of the summation m = 0*)
				newsum = term;
			
				(* Case where zsolutions and terms in the sum al real numbers *)
				If[v > 1/2,
					Do[	term = term * (N[m] - 1./2.) * ((2./z)^(2)) * (N[v] + 1./2. - N[m]); (*we can use the recursive property gamma(z+1)=zgamma(z)*)
						newsum = newsum + term
						,{m, 1, v - 1/2}];
					Return[newsum*(1./Pi) + N[BesselY[v, z]]]
					,
					Return[term*(1./Pi) + N[BesselY[v, z]]]
				]
				, 
				
				v < 0,  (*v < 0*)
				
				(-1)^(-v-1/2) * BesselJ[-v,z] (*Solution for v<0*)
				
				],
					
		z != 0 && !OddQ[2*v], 
		
		prec = OptionValue[PrecisionGoal] /. {Automatic -> 16, MachinePrecision -> 16};
		acc = OptionValue[AccuracyGoal] /. {Automatic -> Infinity, MachinePrecision -> 16};
		
		term = (z/2.)^(v+1) * 1./(N[Gamma[3./2.]] * N[Gamma[v + 3./2.]]);
		k = 1;
		oldsum = 0.;
		newsum = term;
		
		While[ Abs[newsum - oldsum] > Abs[newsum] * 10.^-prec + + 10.^(-acc),
			term = term * (-1)*(z/2.)^2/((k + 1./2.)*(v + 3./2. + k - 1));
			oldsum = newsum;
			newsum = newsum + term;
			k = k + 1;
		];
		newsum
		]
	] /; (Element[v, Reals] && Element[z, Reals])


Print["NumericalFunctions definitions loaded."]

End[] (* End Private Context *)

EndPackage[]