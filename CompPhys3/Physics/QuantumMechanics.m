(* Wolfram Language Package *)

BeginPackage["QuantumMechanics`"]
(* Exported symbols added here with SymbolName::usage *)  

(* Physical Constants *)
planckConstant::usage = "planckConstant represents the Planck's constant"
speedOfLight::usage = "speedOfLight represents the speed of light."
boltzmannConstant::usage = "boltzmannConstant represents the Boltzmann's constant."

(*Definitions:*)
energyDensity::usage = "energyDensity[lambda, T] gives the energy density of a blackbody with wavelength lambda and temperature T."

Begin["`Private`"] (* Begin Private Context *) 

(*=========
	Blackbody radiation energy density
*)

energyDensity[lambda_, T_]:=
	(8 * Pi * planckConstant * speedOfLight) / lambda^5 / (Exp[(planckConstant speedOfLight) / (lambda boltzmannConstant T)] - 1)

Print["QuantumMechanics definitions loaded."]

End[] (* End Private Context *)

EndPackage[]