(* Wolfram Language Package *)

BeginPackage["Differentiation`"]
(* Exported symbols added here with SymbolName::usage *)  

dataDerivative::usage = "dataDerivative[data, n ] computes the nth derivative of a sampled function input data. It is assumed equally spaced abscisas."
FDDerivative::usage = "FDDerivative[f, {x,n}, x0, h] computes the nth derivative of the function f respect to x, using the finite difference formula with step forward"
minErrorDerivative::usage = "minErrorDerivative[f, {x,n}, x0] computes the nth derivative fo the function f to respect to x usinge the central difference formula with and h that gives the minimum error of df."

Begin["`Private`"] (* Begin Private Context *) 

(* ===============
	dataDerivative
	*	Derivative of a sampled function
	*	Uses explicit finite differences formulas
	*	It is assumed equally spaced abscisas
*)

Options[dataDerivative] = {
	"Order" -> 1,
	"FDMethod" -> "Forward"
}

dataDerivative[data_, n_:1, opts:OptionsPattern[]] :=
	dataDerivative[data, 1, opts]

dataDerivative[data_, 1, opts:OptionsPattern[]] :=
	Module[{order, method, x = data[[All, 1]], y = data[[All,2]], newx, dy, h},
		
		h = x[[2]] - x[[1]];
		order = OptionValue["Order"];
		method = OptionValue["FDMethod"];
		
		Switch[{order, method},
			{1, "Forward"},
				dy = (y[[2;;]] - y[[1;;-2]])/h;
				newx = x[[1;;-2]],
			{1,	"Backward"},
				dy	=(y[[2;;]] - y[[1;;-2]])/h;
				newx = x[[2;;-1]]
		];
		Transpose[{newx, dy}]
	]
	
(* ==============
	FDDerivative
	*	Derivative of a function
	*	Uses explicit Finite Differences formulas
*)

Options[FDDerivative] = {
	"Order" -> 1,
	"FDMethod" -> "Forward"
}

FDDerivative[f_, x_, x0_, h_, opts:OptionsPattern[]] :=
	FDDerivative[f, {x, 1}, x0, h, opts]
	
FDDerivative[f_, {x_, 1}, x0_, h_, opts:OptionsPattern[]] :=
	Module[{order, method, df},
		
		order = OptionValue["Order"];
		method = OptionValue["FDMethod"];
		
		Switch[{order, method},
			{1, "Forward"},
				df = ((f /. x -> x0 + h) - (f /. x -> x0))/h,
			{1, "Backward"},
				df = ((f /. x -> x0) - (f /. x -> x0 - h))/h	
		];
		df
	]

Options[minErrorDerivative] = {
	"InitialStep" -> 0.01,
	"ShrinkageFactor" -> 1.4
}


minErrorDerivative::derivative = "This method only works for derivatives of order 1 or 2.";

minErrorDerivative[f_, {x_, n_}, x0_, opts:OptionsPattern[]] := 
		Module[{h0, h1, haux, sFactor, df0, df1, i, err1, err2, dflist},
			
			h0 = OptionValue["InitialStep"]; (*step inicial*)
			sFactor = OptionValue["ShrinkageFactor"]; (*shrinkage factor*)
			h1 = h0/sFactor; (*h0/sFactor*)

			Switch[n,
				1,	(*Primera derivada*)
				
					(*Calculamos el primer error*)
					df0 = ((f /. x -> x0 + h0) - (f /. x -> x0 - h0))/(2*h0); (*df con h0*)
					df1 = ((f /. x -> x0 + h1) - (f /. x -> x0 - h1))/(2*h1); (*df con h0/sFactor*)
					
					err2 = Abs[df1 - df0]/Abs[df0]; (*primer error*)
					err1 = err2*2.; (*valor para que entre al loop*)
					
					i = 2; (*contador del factor shrinkage*)
					dflist = {df0, df1}; (*lista de par de derivada dfi y dfi+1 para calcular el error del step i+1*)
					
					(*Loop:*)
					While[err1 > err2,
						
						haux = h0/sFactor^i; (*Actualizamos el paso h*)
						
						dflist[[1]] = dflist[[2]]; (*dfi+1 pasa al primer termino*)
						dflist[[2]] = ((f /. x -> x0 + haux) - (f /. x -> x0 - haux))/(2*haux); (*calculamos la derivada con el nuevo paso*)
						
						err1 = err2; (*El error old se reemplaza por el new del step anterior*)
						err2 = Abs[dflist[[2]] - dflist[[1]]]/Abs[dflist[[1]]]; (*creamos el nuevo error new*)
						i++; 
					];
					Return[dflist[[1]]], (*Almacenamos la derivada del step anterior ya que esa nos da menor error.*)
				
				2,	(*Segunda Derivada*)
				
					(*Calculamos el primer error*)
					df0 = ((f /. x -> x0 + h0) - 2 * (f /. x -> x0) + (f /. x -> x0 - h0))/(h0^2); (*df con h0*)
					df1 = ((f /. x -> x0 + h1) - 2 * (f /. x -> x0) + (f /. x -> x0 - h1))/(h1^2); (*df con h0/sFactor*)
					
					err2 = Abs[df1 - df0]/Abs[df0]; (*primer error*)
					err1 = err2*2.; (*valor para que entre al loop*)
					
					i = 2; (*contador del factor shrinkage*)
					dflist = {df0, df1}; (*lista de par de derivada dfi y dfi+1 para calcular el error del step i+1*)
					
					(*Loop:*)
					While[err1 > err2,
						
						haux = h0/sFactor^i; (*Actualizamos el paso h*)
						
						dflist[[1]] = dflist[[2]]; (*dfi+1 pasa al primer termino*)
						dflist[[2]] = ((f /. x -> x0 + haux) - 2 * (f /. x -> x0) + (f /. x -> x0 - haux))/(haux^2); (*calculamos la derivada con el nuevo paso*)
						
						err1 = err2; (*El error old se reemplaza por el new del step anterior*)
						err2 = Abs[dflist[[2]] - dflist[[1]]]/Abs[dflist[[1]]]; (*creamos el nuevo error new*)
						i++; 
					];
					Return[dflist[[1]]], (*Almacenamos la derivada del step anterior ya que esa nos da menor error.*)
				_,
					Message[minErrorDerivative::derivative]
			]
		]


Print["Differentiation definitions loaded."]

End[] (* End Private Context *)

EndPackage[]