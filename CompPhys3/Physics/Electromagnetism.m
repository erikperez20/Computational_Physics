(* Wolfram Language Package *)

(* Created by the Wolfram Workbench 21 oct. 2021 *)

BeginPackage["Electromagnetism`"]
(* Exported symbols added here with SymbolName::usage *) 

GradientFieldPlot::usage = "Grafica el gradiente de un campo escalar"
PotentialElectricFieldPlot::usage = "Grafica el campo electrico de un potencial"
chargesPotential::usage = "Calcula el potencial electrico en el punto {x,y} de un conjunto de cargas con magnitud charges"

Begin["`Private`"]
(* Implementation of the package *)

Options[GradientFieldPlot] = Options[VectorPlot]

GradientFieldPlot[f_, {x_, xmin_, xmax_},{y_,ymin_,ymax_}, opts:OptionsPattern[]]:=

VectorPlot[
	Evaluate[D[f, {{x,y}}]] , {x,xmin,xmax}, {y, ymin, ymax}
	,
	opts, VectorScaling -> Automatic (*opts debe ir antes de las opciones default para poder modificarlas al llamar a GradientFieldPlot*)
]

Options[PotentialElectricFieldPlot] = 
	Union[Options[GradientFieldPlot], Options[ContourPlot]]
	
PotentialElectricFieldPlot[potential_, xlim_, ylim_, opts: OptionsPattern[]] :=
	Module[{plot1,plot2},
		
		plot1 = 
			GradientFieldPlot[-potential, xlim, ylim,
				Evaluate@FilterRules[{opts}, Options[GradientFieldPlot]]];
		
		plot2 = 
			ContourPlot[potential, xlim, ylim,
				Evaluate@FilterRules[{opts}, Options[ContourPlot]],
				ContourShading -> None, PlotPoints -> 50];
		
		Show[{plot1,plot2}]
		];

DistancePoints[point1_List?(Depth[#] == 3 && Length[#] == 2 &), point2_List?(Depth[#] == 3 && Length[#] == 2 &)] := Sqrt[Total[(point1 - point2)^2]] /; 
																				QuantityQ[point1[[1]]] && QuantityQ[point1[[2]]] &&
																				QuantityQ[point2[[1]]] && QuantityQ[point2[[2]]]

Options[chargesPotential] = {"Units" -> Automatic}

chargesPotential::DistanceUnitWrong = "The Distance units are written wrong or the units used do not exists in the Mathematica database";
chargesPotential::ChargeUnitWrong = "The Charge units are written wrong or the units used do not exists in the Mathematica database";
chargesPotential::VoltageUnitWrong = "The Voltage units are written wrong or the units used do not exists in the Mathematica database";
chargesPotential::SamePos = "The potential can't be evaluated at a particle's position"

chargesPotential[{x_,y_}, charges_List?(Depth[#] == 2 &), pos_, opts: OptionsPattern[]] := 
	Module[{units, unitValues, UnitsBool, point, pointU, chargesU, posU, CoulombConst, distances},
		units = OptionValue["Units"] /. {Automatic -> {"Distance" -> "Meters", "Charge" -> "Coulombs", "Voltage" -> "Volts"}};
		unitValues = {"Distance", "Charge", "Voltage"} /. units ; 
		
		(*We verify that the input units are known units and correspond to the varaible*)
		UnitsBool = KnownUnitQ[Evaluate[#]] & /@ unitValues ;(*Vector of True of False depending if the unit is a known unit*)
		If[AllTrue[UnitsBool,TrueQ],

			point = {N[x], N[y]};
			pointU = Quantity[point,unitValues[[1]]]; (*point with units*)
			chargesU = Quantity[N[charges],unitValues[[2]]]; (*charges with units*)
			posU = Quantity[N[pos],unitValues[[1]]]; (*pos with units*)

			CoulombConst = Entity["PhysicalConstant", "CoulombConstant"]["Value"]; (*Definition of the Coulomb Constant*)
			distances = DistancePoints[pointU, #] & /@ posU; (*Calculate the distance between each position and the point to evaluate V*)
			
			(*Condition that verifies that the position of evaluation is not the same as one or more coordinate of the charges*)
			If[MemberQ[distances, Quantity[0.,unitValues[[1]]]], Message[chargesPotential::SamePos];Return[{}] ];
			
			UnitConvert[CoulombConst*Total[chargesU * 1/distances], unitValues[[3]]]
			
			,
			If[UnitsBool[[1]] == False,
				Message[chargesPotential::DistanceUnitWrong];
				Return[{}]
			];
			If[UnitsBool[[2]] == False,
				Message[chargesPotential::ChargeUnitWrong];
				Return[{}]
			];
			If[UnitsBool[[3]] == False,
				Message[chargesPotential::VoltageUnitWrong];
				Return[{}]
			]
		]
	]
		
		
		
Print["Electromagnetism definitions loaded."]

End[]

EndPackage[]

