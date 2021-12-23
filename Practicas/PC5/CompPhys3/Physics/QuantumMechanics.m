(* Wolfram Language Package *)

BeginPackage["QuantumMechanics`"]
(* Exported symbols added here with SymbolName::usage *)  

(* Physical Constants *)
planckConstant::usage = "planckConstant represents the Planck's constant"
speedOfLight::usage = "speedOfLight represents the speed of light."
boltzmannConstant::usage = "boltzmannConstant represents the Boltzmann's constant."

(*Definitions:*)
energyDensity::usage = "energyDensity[lambda, T] gives the energy density of a blackbody with wavelength lambda and temperature T."

particleInABoxProbabilityDensity::usage = "determines the probability density function of a particle in a box with gaussian wave packet initial condition"

Begin["`Private`"] (* Begin Private Context *) 

(*=========
	Blackbody radiation energy density
*)

energyDensity[lambda_, T_]:=
	(8 * Pi * planckConstant * speedOfLight) / lambda^5 / (Exp[(planckConstant speedOfLight) / (lambda boltzmannConstant T)] - 1)

(* =================
	Particle in a one dimensional box
	* Computation of the probability density:
		* Initial condition: gaussian wave packet
		* Coefficients of the arbitrary superposition are found numerically
		* Units: m = 1, a = 1, hbar = 1
*)

pi = N@Pi;

(* time independent solutions *)
psiPlus[x_,n_] := Cos[(n - 1/2) pi x]
psiMinus[x_,n_] := Sin[n pi x]

(* energy eigenvalues *)
energyPsiPlus[n_] := (n - 1/2)^2 pi^2 / 2
energyPsiMinus[n_] := n^2 pi^2 / 2

(* initial gaussian wave packet *)
L = 1/4. (* in units  of a *)
k0 = 20. (* in units of 1/a *)

phiPlus[x_] := Cos[k0 x] Exp[-x^2/(4. L^2) / (2. pi L^2)^(1/4)]
phiMinus[x_] := Sin[k0 x] Exp[-x^2/(4. L^2) / (2. pi L^2)^(1/4)]

(* Compute the coefficients of the expression *)
(* With chop: determine a set of coefficients that gives a reasonable approximation of the 
	Gaussian function for the initial wave function that satisfies the boundary conditions *)

cPlus[n_] := cPlus[n] = Chop[2 NIntegrate[Evaluate[psiPlus[x, n] * phiPlus[x]], {x, 0, 1}], 10^(-2)]
cMinus[n_] := cMinus[n] = Chop[2 I NIntegrate[Evaluate[psiMinus[x, n] * phiMinus[x]], {x, 0, 1}], 10^(-2)]

(* wave function at time t.*)
psi[x_, t_] = 
	Sum[
		cPlus[n]*psiPlus[x, n]*Exp[-I energyPsiPlus[n] t] +
			cMinus[n] * psiMinus[x, n] * Exp[-I energyPsiMinus[n] t]
			,
			{n, 1, 14}
	]

psiConjugate[x_, t_] = psi[x, t] /. Complex[p_, q_] :> Complex[p, -q]

(* probability density *)
particleInABoxProbabilityDensity[x_, t_] = 
	psi[x, t] * psiConjugate[x, t] // ExpToTrig // Expand // Chop;






Print["QuantumMechanics definitions loaded."]

End[] (* End Private Context *)

EndPackage[]