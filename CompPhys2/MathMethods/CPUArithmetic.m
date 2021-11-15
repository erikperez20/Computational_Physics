(* Wolfram Language Package *)

BeginPackage["CPUArithmetic`"]
(* Exported symbols added here with SymbolName::usage *)  

absoluteError::usage = "absoluteError[x, y] computes the absolute error of x and y"
relativeError::usage = "relativeError[x, y] computes the relative error of x and y, with respecto to x"
maxExponent::usage = "maxExponent[n] is the max exponent for a e-bit exponent"
minExponent::usage = "minExponent[n] is the min exponent for a e-bit exponent"
minMantissa::usage = "minMantissa[n] is the min mantissa for a n-bit mantissa"
maxMantissa::usage = "maxMantissa[n] is the max mantissa for a n-bit mantissa"
minMachineNumber::usage = "minMachineNumber[n, p] is the min machine number for a n-bit mantissa and a p-bit exponent"
maxMachineNumber::usage = "maxMachineNumber[n, p] is the max machine number for a n-bit mantissa and a p-bit exponent"
machineEpsilon::usage = "machineEpsilon[n] is the machine number for a n-bit mantissa"
inverseGoldPowerD::usage = "inverseGoldPowerD[n] computes n power of the conjugate golden number"
inverseGoldPowerR::usage = "inverseGoldPowerR[n] computes n power of the conjugate golden number"
\[ScriptCapitalF]::usage = "\[ScriptCapitalF][v] controls display of number v"


Begin["`Private`"] (* Begin Private Context *) 

(* ===========
	Function to format numerical values
	* F @ v: ScientificForm with automatic exponent
	* F_e @ v: ScientificForm controlling threshold for exponent
*)

\[ScriptCapitalF] := ScientificForm[#, {17, 16}, NumberPadding -> {"", "0"}] &

Subscript[\[ScriptCapitalF], e_] := NumberForm[#, {20, 17}, ExponentFunction -> (If[-e < # < e, Null, #] &)] &

(* Measure of error *)

absoluteError[x_, y_] := Abs[x - y]

relativeError[x_, y_] := Abs[x - y] / Abs[x]


(* Floating Point Parameters 
	* n: number of digits in the mantissa
	* p: number of digits in the exponent 
*)

maxExponent[p_Integer?Positive] := 2^(p - 1) - 1 // N

minExponent[p_Integer?Positive] := -2^(p - 1) + 2 // N

minMantissa[n_Integer?Positive] := 1. (*1.000...0000 en base 2*)

maxMantissa[n_Integer?Positive] := 2 - 2^-(n - 1) // N (*1.11....111 en base 2*)

minMachineNumber[n_Integer?Positive, p_Integer?Positive] := minMantissa[n] * 2^minExponent[p]

maxMachineNumber[n_Integer?Positive ,p_Integer?Positive] := maxMantissa[n] * 2^maxExponent[p]

machineEpsilon[n_Integer?Positive] := 2^-n // N 

(* ============
	Powers of the conjugate golden number
	* Method 1: direct computation using internal power
	* Method 2: recursive definition with memoization (unstable)
*)

(*Direct Calculation*)
inversePhi = 1./ GoldenRatio

inverseGoldPowerD[n_Integer?Positive] = inversePhi^n;

(*Recursive relation with memoization: unstable algorithm*)

inverseGoldPowerR[0] = 1.;

inverseGoldPowerR[1] = inversePhi;

inverseGoldPowerR[n_Integer?Positive] := 
	(inverseGoldPowerR[n] = inverseGoldPowerR[n - 2] - inverseGoldPowerR[n - 1])


Print["CPUArithmetic definitions loaded."]

End[] (* End Private Context *)

EndPackage[]