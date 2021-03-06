(* Wolfram Language Package *)

BeginPackage["Fourier`"]
(* Exported symbols added here with SymbolName::usage *)  

absFSeries::usage = "Fourier series for the abs function"
stepFSeries::usage = "Fourier series for the step function"
triangleFSeries::usage = "Fourier series for the triangle function."
parableFSeries::usage = "Fourier series for the parable function."
plotPowerSpectrum::usage = "plotPowerSpectrum[f, t, n, tmax] makes a plot of the power spectrum of f[t] using n samples in the interval [0,tmax]"

Begin["`Private`"] (* Begin Private Context *) 

absFSeries[t_, nmax_] := Pi/2 - Sum[(4/((2*n - 1)^2*Pi)) * Cos[(2*n - 1)*t], {n, 1, nmax}]

stepFSeries[t_, nmax_] := Sum[(4/((2*n - 1)*Pi))*Sin[(2*n - 1)*t], {n, 1, nmax}]

parableFSeries[x_, nmax_] := Sum[(1/k^2)*Cos[k*x], {k, 1, nmax}]

triangleFSeries[x_, nmax_] := Sum[(1/(2*k - 1)^2)*Cos[(2*k - 1)*x], {k, 1, nmax}]


(* =========
	plotPowerSpectrum

*)

plotPowerSpectrum[f_, t_, n_, tmax_] :=
	Module[{w, lw, dt, dw, wnyq, tt, ww, ff, PE, PE2},
		
		dt = tmax/n;
		dw = 2*Pi/tmax;
		wnyq = Floor[n/2] dw;
		
		tt = N@Table[t, {t, 0, tmax - dt, dt}];
		ff = N@Table[f, {t, 0, tmax - dt, dt}];
		ww = N@Table[w, {w, 0, wnyq, dw}];
		lw = Length[ww];
		
		PE = Abs[Fourier[ff]];
		PE2 = Transpose[{ww, PE[[1;;lw]]}];
		
		Echo[{dw, wnyq} // N];
		GraphicsRow[
			{
				Show[{ListPlot[Transpose[{tt, ff}], Filling -> Axis],
						Plot[f[t], {t, 0, tmax}, PlotStyle -> {Thin, Red}]}
				],
				ListPlot[PE2, Filling -> Axis, PlotRange -> All]
			}
		]
	]

Print["Fourier definitions loaded."]

End[] (* End Private Context *)

EndPackage[]