(* Wolfram Language Package *)

BeginPackage["ClassicalMechanics`"]
(* Exported symbols added here with SymbolName::usage *)  

(* Physical Constants *)
gravityConstant::usage = "gravityConstant represents the gravitational constant g = 9.81 m/s^2"


(*Definitions: *)
leyMovimiento::usage = "Ley de movimiento del problema presentado"
leyVelocidad::usage = "Velocidad del problema presentado"

Begin["`Private`"] (* Begin Private Context *) 

leyMovimiento[t_, L_, omega_] := (L/2. - gravityConstant/(4*omega^2)) * Exp[omega*t] + (L/2. + gravityConstant/(4*omega^2)) * Exp[-omega*t] + gravityConstant/(2*omega^2) * Sin[omega*t]
leyVelocidad[t_,L_,omega_] := Evaluate@D[leyMovimiento[t, L, omega],t]

Print["ClassicalMechanics definitions loaded"]

End[] (* End Private Context *)

EndPackage[]