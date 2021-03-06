(* Wolfram Language Package *)

BeginPackage["ClassicalMechanics`"]
(* Exported symbols added here with SymbolName::usage *)  

(* Physical Constants *)
gravityConstant::usage = "gravityConstant represents the gravitational constant g = 9.81 m/s^2"


(*Definitions: *)
leyMovimiento::usage = "Ley de movimiento del problema presentado"
leyVelocidad::usage = "Velocidad del problema presentado"

solveSmallCoupledOscillations::usage = "Solve the equation of motion of a system of coupled oscillators in the special case of small oscillations"


Begin["`Private`"] (* Begin Private Context *) 

leyMovimiento[t_, L_, omega_] := (L/2. - gravityConstant/(4*omega^2)) * Exp[omega*t] + (L/2. + gravityConstant/(4*omega^2)) * Exp[-omega*t] + gravityConstant/(2*omega^2) * Sin[omega*t]
leyVelocidad[t_,L_,omega_] := Evaluate@D[leyMovimiento[t, L, omega],t]

Options[solveSmallCoupledOscillations] = {
	"EquilibriumPoint" -> Automatic
}

solveSmallCoupledOscillations[T_, V_, coordinates_List, t_Symbol, opts:OptionsPattern[]] := 
	Module[{q, p, dim, equilibriumPoint, equilibriumRule, Vij, Tij},
		
		dim = Length[coordinates];
		equilibriumPoint = OptionValue["EquilibriumPoint"] /. Automatic -> ConstantArray[0, dim];
		equilibriumRule = Thread[coordinates -> equilibriumPoint];
		
		q = coordinates;
		p = D[coordinates, t];
		
		(*	Potential Energy matrix	 *)
		Vij = Echo[Outer[D[V, #1, #2]&, q, q] /. equilibriumRule, "Potential energy matrix"];
		
		(* Kinetic Energy matrix *)
		Tij = Echo[Outer[D[T, #1, #2]&, p, p] /. equilibriumRule, "Kinetic energy matrix"];
		
		MapAt[#1/Norm[#1] &, Eigensystem[{Vij, Tij}], {2, All}]	
	]
	
	


Print["ClassicalMechanics definitions loaded"]

End[] (* End Private Context *)

EndPackage[]