(* Wolfram Language Package *)

(* Created by the Wolfram Workbench 27 ago. 2021 *)

BeginPackage["WLProgramming`"]
(* Exported symbols added here with SymbolName::usage *) 

f1::usage = "f1 computes something"
f2::usage = "f2 blabla"

Begin["`Private`"]
(* Implementation of the package *)

f1[x_] := x^2 + Sin[x]
f2[x_] := x+6
End[]

EndPackage[]

