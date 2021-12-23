(* Wolfram Language Package *)

BeginPackage["RuleBased`"]
(* Exported symbols added here with SymbolName::usage *)  

sumNaturalsR::usage = "sumNaturalsR calcula la suma de 1 hasta n"
factorialR::usage = "factorialR calcula el factorial de n"
polynomialBaseR::usage = "polynomialBaseF calcula la base polinomial de orden n para la variable x"
reverseSublistsR::usage = "reverseSublistsR invierte el orden las sublistas dado una lista de listas" 
removeSequentialRepetitionsR::usage = "removeSequentialRepetitionsF remueve las secuencias repetidas en una lista"
lispInterpreterR::usage = "lisp interpreter for arithmetic operations"
convertToBytecode::usage = "convierte una lista de 8 bits (base 2) en un entero base 10 o bytecode"
convertFromBytecode::usage = "convierte un bytecode a una lista de 8 bits"
binaryListToBytecodesR::usage = "Convierte una lista de tamanho arbitrario de bits a bytecodes"
byteCodesToBinaryListR::usage = "Convierte una codificacion de bytecodes a la lista correspondiente en bits"

Begin["`Private`"] (* Begin Private Context *) 


sumNaturalsR[n_Integer?Positive] := First[(Range[n] //. {x___, y_, z_, w___} :> {x, y + z, w})]

	(* sumNaturalsR
		-	Rule based programming
		-	Crea una lista semilla de 1 hasta n y aplica secuencialmente la suma de los valores internos
		*)
		
factorialR[n_Integer?Positive] := First[(Range[n] //. {x___, y_, z_, w___} :> {x, y * z, w})]

	(* factorialR
		-	Rule based programming
		-	Crea una lista semilla de 1 hasta n y aplica secuencialmente el producto de los valores internos
		*)

polynomialBaseR[n_Integer?Positive, x_Symbol] := 
		{1} //. {a___, b_} /; !SameQ[b, x^n] :> {a, b, b*x}

	(* polynomialBaseR
		-	Rule based programming
		-	Toma una semilla de {1} y agrega terminos extra en la lista multiplicando el ultimo termino por x 
		*)

(*La condicional verifica que las sublistas tengan profundidad 2, osea sean listas de una dimension*)
reverseSublistsR[lista_List?(!MemberQ[#, False] &@ Map[(Depth[#]==2)&, #] &)] := 
				Replace[lista,l_List:>Reverse[l],1]
	
	(*	reverseSublistsR
		-	Rule based programming
		-	Con Replace reemplaza cada sublista con la lista invertida
		*)	

removeSequentialRepetitionsR[lista_List?(Depth[#] == 2 &)] := 
			lista //. {x___, y_, z_, w___} /; y == z :> {x, y, w} 
	(* removeSequentialRepetitionsR
		-	Rule based programming
		-	busca patrones repetidos y elimina la repeticion secuencialmente
		*)

(*Condicion de que la lista sea de 3 elementos y de profundidad 2 (lista plana de 1 nivel)*)
lispInterpreterR[lista_List?(Depth[#] == 2 && Length[#] == 3 &)] := 
	Replace[lista,
	 {{"/", x__} :> Divide[x],
	  {"+", x__} :> Plus[x],
	  {"-", x__} :> Subtract[x],
	  {"*", x__} :> Times[x]
	  }]
	  
	(*	lispInterpreterR
		-	Rule based programming
	  	-	Aplica Replace a la lista de entrada con distintos operadores dependiendo del primer elemento
	  	*)

(*Condicion de entrada: una lista de longitud = 8, profundidad 2 y con elementos 0 o 1 solamente*)
convertToBytecode[binlist_List?(Length[#] == 8 && Depth[#] == 2 && ContainsOnly[#, {0, 1}] &)] := 
	Module[{modifiedList,bytenumList,bytenum},
		modifiedList = binlist /. x_List :> {{0}, x}; (*Agregamos una sublista con un 0 para crear una estructura semilla*)
		bytenumList = modifiedList //. {{x__},{y_,z___}} :> {{2*x + y}, {z}}; (*El numero de la sublista la multiplicamos por 2 y sumamos
																	 	  el numero siguiente en la lista principal y eliminamos ese ultimo numero*)		
		bytenum = bytenumList /. {{a_},{}} :> a; (*El algoritmo anterior retorna {{num},{}}, usando una regla simple extraemos num*)
		bytenum]

	(*	convertToBytecode
		-	Rule based programming
	  	-	funcion modularizada
	  	*)

DivisionsBy2[number_Integer?Positive] := {number} //. {z___, x_} /; x >= 2 :> {z, x, x/2}
(*Funcion que toma un numero entero y lo divide entre 2 hasta que n/2 >= 2. Retorna una lista del tipo {n,n/2,...,n/2^k} en donde n/2^k < 2*)

convertFromBytecode[bytecode_Integer?(0 <= # && # <= 255 &)] :=
	Module[{baselist, posList, positions},
		(*Creamos una lista de 0s para luego completarla con 1s en las posiciones necesarias:*)
  		baselist = {} //. {x___} /; Length[{x}] < 8 :> {x, 0} ;
		
		(*Se necesitan las posiciones para las cuales reemplazar con 1s, para esto usamos una funcion auxiliar DivisionsBy2 que
		retorna una lista de las maximas divisiones de un numero por multiplos de 2, la longitud de esa lista nos da la posicion en la cual colocar un 1.
		Esta parte del codigo nos retorna una lista de la forma {{pos1,mod1},{pos2,mod2},...} en donde mod es el residuo de dividir sucesivamente
		por 2 el numero entrante y los numeros que resultan.*)
		posList = {{0, bytecode}} //. {ll___, {pos_, mod_}} /; mod > 0 :>
					 {ll, {pos, mod}, {Length[DivisionsBy2[mod]], Mod[mod, 2^(Length[DivisionsBy2[mod]] - 1)]}};
		(*Extraemos solo las posiciones ya que eso necesitamos, estan ubicadas en la primera coordenada de cada sublista*)			 
  		positions = Cases[Rest[posList], {a_, b_} -> {a}];
  		(*Aplicamos replace part a la lista de 0s y reemplazamos en las coordenadas obtenidas con 1s*)
  		ReplacePart[baselist, -positions -> 1]
  		]
  		
  
binaryListToBytecodesR[binlist_List?(Depth[#] == 2 && ContainsOnly[#, {0, 1}] &)] :=
		Module[{length, newbinlist, partitionlist, bytelist},
				length = Length[binlist];(*Obtenemos la longitud de la lista*)
				newbinlist = binlist //. {x___} /; Mod[Length[{x}], 8] != 0 :> {x,0} ;(*Completamos con 0 en caso la longitud de la lista 
																						de numeros binarios no sea multiplo de 8*)
  				(*Agrupamos los numeros de 8 en 8 y los ponemos en sublistas, similar a un Partition*)
  				partitionlist = newbinlist //. {x___List?(Length[#] == 8 &), y__Integer, z__Integer} /; (Length[{y}] == 8 && Length[{z}] >= 0) :> {x, {y}, z};
  				
  				(*El codigo anterior no agrupa los ultimos 8 digitos en una lista y queda de la forma {{1,0,...},{0,1,...},1,0,..,1}, 
				esa ultima secuencia la colocamos en una lista*)
				partitionlist = partitionlist /. {x___List?(Length[#] == 8 &), y__Integer} /; Length[{y}] == 8 :> {x, {y}};
				
				(*Reemplazamos las sublistas de 8 bits por la transformacion a bytecode usando la funcion auxiliar converToBytecode*)
				bytelist = Replace[partitionlist, l_List :> convertToBytecode[l], 1];
				(*Retornamos la longitud de los bits originales y los bytecodes*)
				{length, bytelist}
				]
				
byteCodesToBinaryListR[bytecodes_List?(Depth[#] == 3 && Length[#] == 2 && Depth[#[[2]]] == 2 && (! ListQ[#[[1]]]) &)] :=
		Module[{length, bytelist, binarylists, flattenlist},
				length = bytecodes /. {x_Integer, y_List} :> x; (*Extraemos la longitud de la lista binaria*)
				bytelist = bytecodes /. {x_Integer, y_List} :> y; (*Extraemos la lista de bytecodes*)
				
				(*Reemplazamos los bytes por el numero binario que representa usando la funcion auxiliar*)
				binarylists = Replace[bytelist, l_Integer :> convertFromBytecode[l], 1];
				(*Hacemos un equivalente a flatten*)
				flattenlist = binarylists //. {z___, {x__}, y___} :> {z, x, y};
				(*De la lista aplanada extraemos los primeros "length" numeros, este es el tamano de la lista binaria*)
				flattenlist /. {y__Integer, z__Integer} /; (Length[{y}] == length) :> {y}
  				]				
				
				
Print["Rule Based definitions loaded."]

End[] (* End Private Context *)

EndPackage[]