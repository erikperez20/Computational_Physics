(* Wolfram Language Package *)

BeginPackage["Interpolation`"]
(* Exported symbols added here with SymbolName::usage *)  

lagrangeInterpolate::usage = "Calculates the interpolation of data using Lagrange's method."

Begin["`Private`"] (* Begin Private Context *) 


lagrangeInterpolate[data_, xx_]:=
	Module[{j, k, n = Length[data], x = data[[All , 1]], y = data[[All , 2]]},
		
		Sum[
			y[[k]] * Product[(xx - x[[j]]) / (x[[k]] - x[[j]]), {j, 1, k-1}] * 
				Product[(xx - x[[j]]) / (x[[k]] - x[[j]]), {j, k + 1, n}]
			,
			{k, 1, n}
		]
	] /; MatrixQ[data,NumericQ] &&  Dimensions[data][[2]] == 2 && (NumericQ[xx] || VectorQ[xx, NumericQ])

lagrangeInterpolate[data_, xx_Symbol] :=
	Module[{j, k, n = Length[data], x = data[[All , 1]], y = data[[All , 2]]},
		
		Sum[
			y[[k]] * Product[(xx - x[[j]]) / (x[[k]] - x[[j]]), {j, 1, k-1}] * 
				Product[(xx - x[[j]]) / (x[[k]] - x[[j]]), {j, k + 1, n}]
			,
			{k, 1, n}
		]
	] /; MatrixQ[data,NumericQ] &&  Dimensions[data][[2]] == 2

(* =======================
	Overloading Interpolation and InterpolationFunction
	*	Add Lagrange's method to Interpolation
	*	Add boxes representation to InterpolatingFunction
*)

Unprotect[Interpolation, InterpolatingFunction]

Interpolation[data_?VectorQ, opts:OptionsPattern[]] := 
	InterpolatingFunction[Transpose[{Range[Length[data]], data}], "Lagrange"] /;
		Quiet@(OptionValue[Method] == "Lagrange")
		
Interpolation[data_?MatrixQ, opts:OptionsPattern[]] :=
	InterpolatingFunction[data, "Lagrange"] /;
		Dimensions[data][[2]] == 2 && Quiet@( OptionValue[Method] == "Lagrange")

InterpolatingFunction /: MakeBoxes[obj: InterpolatingFunction[data_, method_], form:(StandardForm|TraditionalForm)] :=
	Module[{above, below},
		above = {
			BoxForm`SummaryItem[{"Domain: ", MinMax@data[[All,1]]}],
			BoxForm`SummaryItem[{"Domain: ", "scalar|vector"}]
		};
		below = {
			BoxForm`SummaryItem[{"Order: ", Length[data] - 1}],
			BoxForm`SummaryItem[{"Method: ", method}]
		};
		BoxForm`ArrangeSummaryBox[
			InterpolatingFunction, obj, None, above, below, form,
				"Interpretable" -> Automatic
		]
		
	];
	
InterpolatingFunction[data_, "Lagrange"][x_] := lagrangeInterpolate[data, x]

Protect[Interpolation, InterpolatingFunction]


Print["Interpolation definitions loaded."]

End[] (* End Private Context *)

EndPackage[]