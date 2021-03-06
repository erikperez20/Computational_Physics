(* Wolfram Language Package *)

BeginPackage["AnalysingData`"]
(* Exported symbols added here with SymbolName::usage *)  

importDATFile::usage = "importDatFile[file] imports and interprets a table of numerical data."
interpolateDATFile::usage = "interpolationDATFile[file] interpolates data in a file and gives an association with it together with extra information."
filterNoisySignal::usage = "filter noisy signal function"

Begin["`Private`"] (* Begin Private Context *) 

Options[importDATFile] := 

(* ======
	importDATFile
	*	Calls internally Import with 2nd argument {"Table", "Data"}
	*	Interprets the file independently of the *.DAT extension
	*	Supports all options for "Table"
	*	New options added: Transposed, Rows and Columns
*)

Options[importDATFile] = {
	(*Importer Table options*)
	CharacterEncoding -> "ASCII",
	"FieldSeparators" -> {" ", "\t"},
	"HeaderLines" -> 0,
	"IgnoreEmptyLines" -> False,
	"LineSeparators" -> {"\r\n", "\n", "\r"},
	"NumberPoint" -> ".",
	"NumberSigns" -> {"-","+"},
	"Numeric" -> True,
	"RepeatedSeparators" -> True,
	
	(* Additional options *)
	"Transposed" -> False,
	"Rows" -> All,
	"Columns" -> All
}

importDATFile[file:(_String|_File), opts:OptionsPattern[]] := 
	Module[
		{data, tableOpts},
		
		tableOpts = {CharacterEncoding, "FieldSeparators", "HeaderLines",
			"IgnoreEmptyLines", "LineSeparators", "NumberPoint", "NumberSigns", 
			"Numeric", "RepeatedSeparators"};
		
		If[OptionValue["Rows"] === All && OptionValue["Columns"] === All,
			(* faster than All, All*)
			data = Import[file, {"Table", "Data"},
						FilterRules[{opts}, tableOpts]]
			,
			(* slow for All, All*)
			data = Import[file, {"Table", "Data", OptionValue["Rows"],
						OptionValue["Columns"]}, FilterRules[{opts}, tableOpts]]

		];
		
		If[ArrayDepth[data] == 2 && TrueQ[OptionValue["Transposed"]], data = Transpose[data]];
		
		Echo[Text@Row[#, "x"]&@Dimensions[data], "Dimensions: "];
		data
	]

(* ======
	interpolateDATFile
	*	Call Interpolation to do the interpolation
	*	Gives an association with: InterpolatedFunction, Data and Statistics
*)

Options[interpolateDATFile] = 
	Join[Options[Interpolation], Options[importDATFile]]

interpolateDATFile::nonnum  = "Non numerical array of data"

interpolateDATFile[file:(_File|_String), opts: OptionsPattern[]] :=
	Module[{data, if},
		
		data = importDATFile[file, FilterRules[{opts}, Options[importDATFile]]];
		
		If[ArrayQ[data, 1|2, NumericQ],
			if = Interpolation[data, FilterRules[{opts}, Options[Interpolation]]]
			,
			Message[interpolateDATFile::nonnum];
			$Failed
		];
		
		Association["InterpolatedFunction" -> if, "Data" -> data,
			"Statistics" -> Association["Mean" -> Mean[data[[All,2]]]]	
		]
	]
	
(* ==========
	filterNoisySignal
	*	Filter a noisy signal using the DFT
	*	Uses dynamic features to set the threshold
*)

filterNoisySignal[data_] := 
	Module[{x, signal, fourierSignal, filteredFourierSignal,
		filteredSignal, threshold, nm = 0},
		
		x = data[[All, 1]];
		signal = data[[All, 2]];
		
		fourierSignal = Fourier[signal];
		
		DialogInput[{
			Panel@
				ListPlot[Abs[fourierSignal], PlotRange -> All,
					AspectRatio -> 0.6, Frame -> True, Filling -> Axis],
			TextCell["Set threshold: "],
			InputField[Dynamic[nm], Number],
			DefaultButton[DialogReturn[threshold = nm]]
			}
		];
		
		filteredFourierSignal = Chop[fourierSignal, threshold];
		
		filteredSignal = Chop[InverseFourier[filteredFourierSignal]];
		
		Transpose[{x, filteredSignal}]
	]

Print["AnalysingData definitions loaded."]

End[] (* End Private Context *)

EndPackage[]