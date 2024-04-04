(* ::Package:: *)

(* ::Input:: *)
(*BeginPackage["max"]*)


(* ::Input::Initialization:: *)
ReplaceInRows[matrix_, oldValue_, newValue_] := Replace[matrix, oldValue->newValue, {2}]


(* ::Input::Initialization:: *)
CreateFileName[filename_] := FileNameJoin[{NotebookDirectory[], filename}]


(* ::Input::Initialization:: *)
CreateFileNameForRoute[filename_] := FileNameJoin[{NotebookDirectory[], "Routes", filename}]


(* ::Input::Initialization:: *)
ExportMaxPlusMatrix[filename_, matrix_]:= Module[{m},
	m = ReplaceInRows[matrix, eps, -1];
	Export[CreateFileName[StringJoin[filename, ".hdf"]],m]
];


(* ::Input::Initialization:: *)
ImportMaxPlustMatrix[filename_]:= Module[{m},
	m = Import[CreateFileName[StringJoin[filename, ".hdf"]],{"Datasets","Dataset1"}];
	ReplaceInRows[m, -1, eps];
	ReplaceInRows[m, -1., eps]
];


(* ::Input::Initialization:: *)
DataToAssociation[filename_] := Module[{data, transpose, timeData, timeDiffTable, maxModeTimes, stopPairs, association, waiting, travelTimeAssociation, i, allStops },
	data = Import[CreateFileNameForRoute[filename],"Table"];
	transpose = Transpose[data];
	timeData=Transpose[Rest[transpose]/. minutes_Integer:>TimeObject[{Quotient[minutes,100],Mod[minutes,100],0}]];
	timeDiffTable = Table[Table[timeData[[All,j]][[i]]-timeData[[All,j]][[i-1]], {i, 2, Length[timeData]}], {j, 1, Length[Transpose[timeData]]}];
	maxModeTimes = Table[Max[Commonest[timeDiffTable[[All, i]]]], {i, 1, Length[timeDiffTable[[1]]]}];
	stopPairs = Table[List[First[transpose][[i-1]],First[transpose][[i]]], {i, 2, Length[maxModeTimes]+1}];
	association = Association[Table[stopPairs[[i]]->maxModeTimes[[i]], {i, 1, Length[stopPairs]}]];
	waiting = Association[Table[Keys[association][[i]]->
		If[
			(StringTake[Keys[association][[i]][[1]], -4] == "_arr" && StringTake[Keys[association][[i]][[2]], -4] == "_dep") ,
			association[[Key[Keys[association][[i]]]]],
	Quantity[0,"Minutes"]],
		{i, 1, Length[Keys[association]]}
]
];

	travelTimeAssociation = Module[{cleaned},
		cleaned = Association[];
		i = 0;
		While[i <Length[association],
			i++;
			If[waiting[[i]]==0Quantity[1,"Minutes"] && (StringTake[Keys[association][[i]][[1]], -4] == "_dep" || StringTake[Keys[association][[i]][[2]], -4] == "_arr"),
			If[StringTake[Keys[association][[i]][[1]], -4] == "_dep",
			AppendTo[cleaned, List[StringDrop[Keys[association][[i]][[1]], -4],Keys[association][[i]][[2]]]-> association[[Key[Keys[association][[i]]]]]],
			AppendTo[cleaned, List[Keys[association][[i]][[1]],StringDrop[Keys[association][[i]][[2]], -4]]-> association[[Key[Keys[association][[i]]]]]]
			],
			If [waiting[[i]]==0Quantity[1,"Minutes"],
			cleaned[Keys[association][[i]]] = association[[Key[Keys[association][[i]]]]],
			Nothing[]],
			Nothing[]];
		];
		cleaned
	];

	allStops = List[];
	AppendTo[allStops, Keys[travelTimeAssociation][[1]][[1]]];
	Do[AppendTo[allStops, x[[2]]],{x, Keys[travelTimeAssociation]}];
	(*List[travelTimeAssociation, waiting, allStops]*)
	travelTimeAssociation
];


(* ::Input::Initialization:: *)
x_\[CirclePlus]y_:=Max[x,y]


(* ::Input::Initialization:: *)
x_ \[CircleTimes]y_:=x+y


(* ::Input::Initialization:: *)
mplus[x_,y_]:=MapThread[Max,{x,y}, 2]


(* ::Input::Initialization:: *)
mtimes[x_,y_]:=Inner[Plus,x,y,Max]


(* ::Input::Initialization:: *)
mpower[x_,p_]:= Module[{res, i},
	If[p>=1,
		res = x,
		res = EMatrix[Length[x]]];
	For[i = 2, i<=p, i++,
		res = mtimes[res, x]
	];
	res
]


(* ::Input::Initialization:: *)
mpowerDynamic[x_, p_, kpx_] := Module[{closestBelow, pow, pows, i},
	pows = kpx;
	If[p>1,
		closestBelow =Max@Select[Keys[kpx],#<=p&];
		pow =pows[closestBelow];
		For[i = closestBelow+1, i<=p, i++,
			pow = mtimes[pow, x];
			AppendTo[pows, i->pow];
		];,
		If[p <=0,
			pow=EMatrix[Length[x]],
			pow=x
			];
		];
	List[pow, pows]
]


(* ::Input::Initialization:: *)
scalarplus[x_,y_]:=Outer[Max,{x},y]


(* ::Input::Initialization:: *)
eps=-Infinity


(* ::Input::Initialization:: *)
EpsilonMatrix[n_] := Table[Table[eps, {col, 1, n}], {row, 1, n}]
(* Returns an n x n matrix filled with Epsilon *)


(* ::Input::Initialization:: *)
EMatrix[n_]:= Table[Table[If[col == row, 0, eps], {col, 1, n}], {row, 1, n}]
(* Returns an n x n identity matrix *)


(* ::Input::Initialization:: *)
MakeNicePlaceName[rn_, s1_, s2_] :=ToString[StringJoin["p:",rn,":", s1, "+", s2]]
(* Returns a formatted place name *)


(* ::Input::Initialization:: *)
MakeNiceTransitionName[rn_,q_] := ToString[StringJoin["q:",rn,":", q]]
(* Returns a formatted transition name *)


(* ::Input::Initialization:: *)
MergeDirections[forward_, reverse_] := Module[{f, r, merged, nk, xS, start, end},
(* Merges the forward and backward (arbitary) directions of a bus route *)
	f = DataToAssociation[forward];
	start = Keys[f][[1]][[1]];
	r = DataToAssociation[reverse];
	end = Keys[r][[1]][[1]];

	Do[
		nk ={StringJoin[x[[1]], If[x[[1]] ==start,"_end","_fwd"]], StringJoin[x[[2]], If[x[[2]] ==end,"_end","_fwd"]]};
		f[nk] = f[x];
		f[x] =.,
	{x, Keys[f]}
	];

	Do[
		nk ={StringJoin[x[[1]], If[x[[1]] ==end,"_end","_rev"]],StringJoin[x[[2]],  If[x[[2]] ==start,"_end","_rev"]]};
		r[nk] = r[x];
		r[x] =.,
	{x, Keys[r]}
	];

	merged = Join[f, r];
	merged
]


(* ::Input::Initialization:: *)
GetTransitionStopName[transitionString_] := StringDrop[StringSplit[transitionString, ":"][[-1]],-4];
(* Returns the transition name from a "nice" formatted transition *)


(* ::Input::Initialization:: *)
GetTransitionStop[transitionString_]:=StringSplit[transitionString, ":"][[-1]];
(* Returns the transition stop, including route direction from a "nice" formatted transition *)


(* ::Input::Initialization:: *)
GetTransitionRoute[transitionString_] :=StringSplit[transitionString, ":"][[2]];(* Returns the transition route number from a "nice" formatted transition *)


(* ::Input::Initialization:: *)
Algorithm821[lineDataFile_, routeNumbers_, cycleTime_:Quantity[60,"Minutes"]]:=
(*Line data file is a list of associations, where each asscoiation corresponds to a merged version of the TT association for the forward and backward journey of a given route. So each association represents a route*)
Module[{TAPsFromRoute, Synchronization, NumberRoutes, temps, sync},

	NumberRoutes[] := Module[{indexedAssociations},
	(* returns an association with route number as key and travel time associations (route data) as values *)
		indexedAssociations = Association[];
		For[i=1,i<=Length[routeNumbers], i++,
			indexedAssociations[routeNumbers[[i]]]= lineDataFile[[i]];
		];
		indexedAssociations
	];

	TAPsFromRoute[routeNumber_, routeAssociation_] :=Module[{startStop, endStop, cumulativeTime,nextStopCumulativeTime, modCumulative, modNextCumulative,tokens, finalHoldingTime, transitions, places, arcs},
		(* returns the transitions, arcs, and place of an individual route *)
		transitions = List[];
		places = Association[];
		arcs = List[];
		startStop = Keys[routeAssociation][[1]][[1]];
		endStop = Keys[routeAssociation][[-1]][[1]];
		AppendTo[transitions,MakeNiceTransitionName[routeNumber, startStop]];
		cumulativeTime = 0Quantity[1,"Minutes"];
		Do[
			AppendTo[transitions,MakeNiceTransitionName[routeNumber, pair[[2]]]];
			nextStopCumulativeTime =cumulativeTime + routeAssociation[[Key[pair]]];
			modCumulative = Mod[cumulativeTime, cycleTime];
			modNextCumulative = Mod[nextStopCumulativeTime, cycleTime];
			tokens = Ceiling[(routeAssociation[[Key[pair]]] + modCumulative - modNextCumulative)/cycleTime];
			AppendTo[places,  MakeNicePlaceName[routeNumber, pair[[1]],pair[[2]]]->List[routeAssociation[[Key[pair]]], tokens]];
			cumulativeTime = nextStopCumulativeTime;
			AppendTo[arcs, MakeNiceTransitionName[routeNumber, pair[[1]]]->Keys[places][[-1]]];
			AppendTo[arcs, Keys[places][[-1]]->MakeNiceTransitionName[routeNumber, pair[[2]]]];
			,
			{pair, Keys[routeAssociation][[1;;-2]]}
		];
		finalHoldingTime =routeAssociation[[Key[List[endStop, StringReplace[startStop, {"fwd"->"rev"}]]]]];
		nextStopCumulativeTime =cumulativeTime + finalHoldingTime;
		modCumulative = Mod[cumulativeTime, cycleTime];
		modNextCumulative = Mod[nextStopCumulativeTime, cycleTime];
		tokens = Ceiling[(finalHoldingTime + modCumulative - modNextCumulative)/cycleTime];
		AppendTo[places, MakeNicePlaceName[routeNumber,endStop, startStop]->List[finalHoldingTime, tokens] ]; 
		AppendTo[arcs, MakeNiceTransitionName[routeNumber, endStop]->MakeNicePlaceName[routeNumber, endStop, startStop]];
		AppendTo[arcs, MakeNicePlaceName[routeNumber, endStop, startStop]->MakeNiceTransitionName[routeNumber, startStop]];
		List[transitions, arcs, places]
	];

	Synchronization[routePlaces_] := Module[{arcs, places, indexedRoutes, placeWeWant},
	(* returns the arcs and places between routes with shared stops *)
		indexedRoutes = NumberRoutes[];
		arcs=List[];
		places=Association[];
		Do[
			Do[
				Do[
					Do[
						If[assoc1 != assoc2,
							If[StringDrop[key1[[2]],-4]==StringDrop[key2[[2]],-4],
							(* add arc from previous stop on origin line to new place *)
							(* add arc from new place to shared stop on the other line *)
							placeWeWant = MakeNicePlaceName[assoc2, key2[[1]], key2[[2]] ];
							AppendTo[places, MakeNicePlaceName[StringJoin[assoc2, "->", assoc1], key2[[1]], key1[[2]]]->routePlaces[[placeWeWant]]];
							AppendTo[arcs, MakeNiceTransitionName[assoc2,key2[[1]]]->Keys[places][[-1]]];
							AppendTo[arcs, Keys[places][[-1]]->MakeNiceTransitionName[assoc1, key1[[2]]]];,
							Nothing[]
							];
							Nothing[]
						];,
						{key2,Keys[indexedRoutes[[Key[assoc2]]]]}
					],
					{key1,Keys[indexedRoutes[[Key[assoc1]]]]}
				],
				{assoc2,Keys[indexedRoutes]}
			],
			{assoc1,Keys[indexedRoutes]}
		];
		List[arcs, places]
	];

	transitions = List[];
	places = Association[];
	arcs = List[];
	
	(* find transitions, arcs, and places for all routes *);
	For[i=1, i<=Length[lineDataFile], i++,
		temps= TAPsFromRoute[routeNumbers[[i]], lineDataFile[[i]]];
		transitions = Join[transitions, temps[[1]]];
		arcs = Join[arcs, temps[[2]]];
		places = Join[places, temps[[3]]];
	];
	
	(* find arcs and places between routes *);
	sync = Synchronization[places];
	arcs = Join[arcs, sync[[1]]];
	places = Join[places, sync[[2]]];
	
	List[transitions, arcs, places]
]


(* ::Input::Initialization:: *)
MatrixFromPetriNet[transitions_, arcs_, places_] := Module[{AMatrices, GetPlaceBetween, A0Star, MatrixABlocks, ATwiddle},
	(*reccurence relation*)
	GetPlaceBetween[t1_, t2_] :=Module[{p},
	If[
	GetTransitionRoute[t1]==GetTransitionRoute[t2],
	
	p =MakeNicePlaceName[GetTransitionRoute[t1], GetTransitionStop[t1], GetTransitionStop[t2]],
	
	p = MakeNicePlaceName[StringJoin[GetTransitionRoute[t1], "->", GetTransitionRoute[t2]], GetTransitionStop[t1], GetTransitionStop[t2]]
	];
	p
	];
	
	AMatrices[]:=Module[{maxTokens, p, row, matrix, matrices},
		maxTokens = 0;
		Do[
			If[x[[2]]>maxTokens,
				maxTokens=x[[2]],
				Nothing[]
			],
			{x, places}];
		
		matrices = List[];
		
		For[m=0, m<=maxTokens, m++, 
			matrix = List[];
			Do[
				row =List[];
				Do[
					p =GetPlaceBetween[j,i];
					If[MemberQ[Keys[places], p],
						p = places[[p]];
						If[p[[2]]==m,
							AppendTo[row, (p[[1]]/Quantity[1,"Minutes"])],
							AppendTo[row, eps ]
						],
						AppendTo[row, eps ];
					];
		,
		{j, transitions}];
		  AppendTo[matrix, row];
			,
			{i, transitions}];
			AppendTo[matrices, matrix];
		];
		matrices
	];
	
	A0Star[A0_] :=Module[{R, knownPowers,powerAndKnown, mp},
		R =EMatrix[Length[A0]];
		knownPowers=Association[1->A0];
		For[l=1, l<=Length[transitions], l=l+1,
			powerAndKnown= mpowerDynamic[A0, l, knownPowers];
			mp = powerAndKnown[[1]];
			knownPowers =powerAndKnown[[2]];
			Print[Max[Keys[knownPowers]]];
			R=mplus[R,mp];
		];
		R
	];
	
	MatrixABlocks[ AMatrixList_, A0S_] :=Module[{s, blocks, a},
		blocks = List[];
		Do[
			a= mtimes[A0S, A];
			AppendTo[blocks,a],
			{A, Drop[AMatrixList, 1]}
		];
		blocks
	];
	
	ATwiddle[blocks_]:=Module[{atwid, flatBlocks, row, rows, blockLen},
		If[Length[blocks]==1,
			atwid = blocks[[1]],
			blockLen = Length[blocks[[1]]];
			flatBlocks = ArrayFlatten[{blocks}];
			rows=flatBlocks;
			For [i=1, i<=Length[blocks]-1,i++,
				row =Table[EpsilonMatrix[blockLen], Length[blocks]];
				row =ReplacePart[row, i->EMatrix[blockLen]];
				rows =Join[rows, ArrayFlatten[{row}], 1];
			];
			atwid =rows;
		];
		atwid
	];
	
	Comp[]:=Module[{matrixList, A0S, matrixABlocks, matrix},
		matrixList = AMatrices[];
		A0S = A0Star[matrixList[[1]]];
		matrixABlocks = MatrixABlocks[matrixList, A0S];
		matrix = ATwiddle[matrixABlocks];
		
		matrix
	];
	Comp[]
]


(* ::Input::Initialization:: *)
CreatePetriNetFromTAPs[TAPs_] := Module[{placesList, transitionsList, arcsList, initialMarkings, petriNet},
	placesList=Keys[TAPs[[3]]];
	transitionsList=TAPs[[1]];
	arcsList=TAPs[[2]];
	initialMarkings=#[[2]]&/@Values[TAPs[[3]]];
	petriNet=ResourceFunction["MakePetriNet"][placesList,transitionsList,arcsList, initialMarkings];
	petriNet["LabeledGraph"]
]


(* ::Input::Initialization:: *)
ChangeNonZeroValues[matrix_,newValue_]:=matrix/. x_/;x!=eps:>newValue


(* ::Input::Initialization:: *)
MakeAdjacencyGraph[mtx_, TAPs_] := Module[{noZeros, noInfi},
	noZeros = ChangeNonZeroValues[mtx, 1];
	noInfi = ReplaceInRows[noZeros, -eps, 0] ;
	AdjacencyGraph[noInfi, {VertexLabels->Table[i->StringJoin[ToString[i], " - ",TAPs[[1]][[i]]],{i,1,Length[TAPs[[1]]]}]}]
]



(* ::Input::Initialization:: *)
MakeStateVector[mtx_, replace_ : 0, random_ : False] := 
 Module[{length, vector, val}, length = Length[mtx];
  vector = Table[eps, length];
  For[i = 1, i <= length, i++,
   If[replace == 0 && random, replace = 1, Nothing[]];
   If[Max[mtx[[All, i]]] >= 0,
    If[random, vector[[i]] = RandomInteger[{0, replace}], 
     vector[[i]] = replace];
    Nothing[]]
   ];
  vector
  ]


TimeTable[mtx_, vec_, transitions_, length_ : 5] := Module[{xNext, list, kpx},
	kpx = Association[1 -> mtx];
	list = List[vec];
	i = 2;
	xNext = mtimes[mpower[mtx, 1], vec];
	AppendTo[list, xNext];
	While[i < length,
		powerAndKnown = mpowerDynamic[mtx, i, kpx];
		xNext = mtimes[powerAndKnown[[1]], vec];
		kpx = powerAndKnown[[2]];
		AppendTo[list, xNext];
		i++;
];
	Join[{transitions}, list]
]


SplitByRoutes[timeTable_, routeNames_] := Module[{ttbs, ttb},
ttbs=List[];
Do[
ttb = List[];
For[u=1, u<=Length[Transpose[timeTable]], u++,
If[GetTransitionRoute[timeTable[[1,u]]] == r,
AppendTo[ttb, timeTable[[All,u]]],
Nothing[]];
];
AppendTo[ttbs, ttb];,
{r, routeNames}
];
ttbs
]


(* ::Input:: *)
(**)


(* ::Input:: *)
(*EndPackage[]*)
(**)
