(* ::Package:: *)

BeginPackage["FortranLink`"]


LibraryLinkWrapperCCodeGenerate::usage = "LibraryLinkWrapperCCodeGenerate[cdeclaration, libraryFunctionName] generate the C code for the LibraryLink wrapper.";


CompileFortranLibraryLink::usage = "CompileFortranLibraryLink[wrapperPath, fortranPath, fOptions, cOptions] compiles the fortran subroutine together with the LibraryLink Wrapper.";

CreateFortranLibrary::usage = "CreateFortranLibrary[source,declaration,name] compiles a string of Fortran code and creates a library file, name.
CreateFortranLibrary[{Subscript[file, 1],\[Ellipsis]},declaration,name] compiles a number of C and Fortran source files and creates a library file, name.
";

(*"Example:*)
(*fortranSubroutineLink[{'rst1.f','pseudo_pot_He.f90','calAtomicStateEnergy.f90'},'void calatomicstateenergy(char atomName[], int* nState, int* nr, int* lwant, double eigenEnergy[], double eigenState[])','myfun']*)
(*";*)

$FortranLinkCCompiler::usage = "$FortranLinkCCompiler sets the default C compiler to use for operating on C code.";
$FortranLinkFCompiler::usage = "$FortranLinkFCompiler sets the default C compiler to use for operating on Fortran code.";

LinkFortranSubroutine::usage = "LinkFortranSubroutine[fortranSrc,declaration] links Fortran subroutine into Mathematica through LibraryLink.
LinkFortranSubroutine[fortranSrc,declaration,codeSnippet] links Fortran subroutine into Mathematica through LibraryLink, insert codeSnippet into the wrapper code.";

Begin["`Private`"]
Needs["SymbolicC`"];

(*setup the compiler path*)

$FortranLinkCCompiler = {"Compiler" -> "intel", "Path" -> "icc"};
$FortranLinkFCompiler = {"Compiler" -> "intel", "Path" -> "ifort"};


(* wolfram include file path*)
wflibPath=FileNameJoin[{$InstallationDirectory, "SystemFiles/IncludeFiles/C/WolframLibrary.h"}];

intStarQ[str_] := StringMatchQ[str, "int\\*" ~~ ___];(* int* type *)
doubleStarQ[str_] := StringMatchQ[str, "double\\*" ~~ ___];(* double* type *)
complexStarQ[str_] := StringMatchQ[str, "complex\\*" ~~ ___];(* complex* type *)
intArrayQ[str_] := StringMatchQ[str, "int" ~~ ___ ~~ "[]"];(* int [] type *)
doubleArrayQ[str_] := StringMatchQ[str, "double" ~~ ___ ~~ "[]"];(* double [] type *)
complexArrayQ[str_] := StringMatchQ[str, "complex" ~~ ___ ~~ "[]"];(* complex [] type *)
charArrayQ[str_] := StringMatchQ[str, "char" ~~ ___ ~~ "[" ~~ ___ ~~ "]"];(* char* type *)


getArgumentForIntegerStar[arg_String, argIndex_Integer] := CStatement[CDeclare["mint", CAssign[StringReplace[arg, "int*" ~~ x_ :> x], CCall["MArgument_getInteger", {CArray["Args", {ToString@argIndex}]}]]]];
getArgumentForDoubleStar[arg_String, argIndex_Integer] := CStatement[CDeclare["double", CAssign[StringReplace[arg, "double*" ~~ x_ :> x], CCall["MArgument_getReal", {CArray["Args", {ToString@argIndex}]}]]]];
getArgumentForComplexStar[arg_String, argIndex_Integer] := CStatement[CDeclare["mcomplex", CAssign[StringReplace[arg, "complex*" ~~ x_ :> x], CCall["MArgument_getComplex", {CArray["Args", {ToString@argIndex}]}]]]];

getArgumentForIntegerArray[arg_String, argIndex_Integer] := Module[{varName, tenserName},
  varName = StringReplace[arg, "int" ~~ x___ ~~ "[]" :> StringTrim@x];
  tenserName = "_t" <> varName;
  {
    CDeclare["MTensor", CAssign[tenserName, CCall["MArgument_getMTensor", {CArray["Args", {ToString@argIndex}]}]]],
    CDeclare["mint*", CAssign[varName, CCall["libData->MTensor_getIntegerData", {tenserName}]]]
  }];

getArgumentForDoubleArray[arg_String, argIndex_Integer] := Module[{varName, tenserName},
  varName = StringReplace[arg, "double" ~~ x___ ~~ "[]" :> StringTrim@x];
  tenserName = "_t" <> varName;
  {
    CDeclare["MTensor", CAssign[tenserName, CCall["MArgument_getMTensor", {CArray["Args", {ToString@argIndex}]}]]],
    CDeclare["double*", CAssign[varName, CCall["libData->MTensor_getRealData", {tenserName}]]]
  }];

getArgumentForComplexArray[arg_String, argIndex_Integer] := Module[{varName, tenserName},
  varName = StringReplace[arg, "complex" ~~ x___ ~~ "[]" :> StringTrim@x];
  tenserName = "_t" <> varName;
  {
    CDeclare["MTensor", CAssign[tenserName, CCall["MArgument_getMTensor", {CArray["Args", {ToString@argIndex}]}]]],
    CDeclare["mcomplex*", CAssign[varName, CCall["libData->MTensor_getComplexData", {tenserName}]]]
  }];

getArgumentForCharArray[arg_String, argIndex_Integer] := Module[{varName, charLength},
  varName = StringReplace[arg, "char" ~~ x___ ~~ "[" ~~ ___ ~~ "]" :> StringTrim@x];
  charLength = StringReplace[arg, "[" ~~ x___ ~~ "]" :> StringTrim@x];
  {
    CDeclare["char* ", CAssign[varName, CCall["MArgument_getUTF8String", {CArray["Args", {ToString@argIndex}]}]]]
  }];



getArgument[arg_String, argIndex_Integer] := Module[{},
  Which[
    intStarQ[arg], getArgumentForIntegerStar[arg, argIndex],
    doubleStarQ[arg], getArgumentForDoubleStar[arg, argIndex],
    complexStarQ[arg], getArgumentForComplexStar[arg, argIndex],
    intArrayQ[arg], getArgumentForIntegerArray[arg, argIndex],
    doubleArrayQ[arg], getArgumentForDoubleArray[arg, argIndex],
    complexArrayQ[arg], getArgumentForComplexArray[arg, argIndex],
    charArrayQ[arg], getArgumentForCharArray[arg, argIndex]
  ]
];


getAllArguments[args_] := Module[{},
  Table[
    getArgument[args[[i]], i - 1]
    , {i, 1, Length@args}]
]


disownIntegerTensor[arg_] := Module[{varName, tenserName},
  varName = StringReplace[arg, "int" ~~ x___ ~~ "[]" :> StringTrim@x];
  tenserName = "_t" <> varName;
  CStatement[CCall["libData->MTensor_disownAll", tenserName]]
]


disownDoubleTensor[arg_] := Module[{varName, tenserName},
  varName = StringReplace[arg, "double" ~~ x___ ~~ "[]" :> StringTrim@x];
  tenserName = "_t" <> varName;
  CStatement[CCall["libData->MTensor_disownAll", tenserName]]
]


disownComplexTensor[arg_] := Module[{varName, tenserName},
  varName = StringReplace[arg, "complex" ~~ x___ ~~ "[]" :> StringTrim@x];
  tenserName = "_t" <> varName;
  CStatement[CCall["libData->MTensor_disownAll", tenserName]]
]


disownArgument[arg_String] := Module[{},
  Which[
    intArrayQ[arg], disownIntegerTensor[arg],
    doubleArrayQ[arg], disownDoubleTensor[arg],
    complexArrayQ[arg], disownComplexTensor[arg]
  ]
]


disownAllArguments[args_] := disownArgument /@ Select[args, intArrayQ[#] || doubleArrayQ[#] || complexArrayQ[#]&]



declare = "
DLLEXPORT mint WolframLibrary_getVersion(){
  return WolframLibraryVersion;
}
DLLEXPORT int WolframLibrary_initialize(WolframLibraryData libData){
  return 0;
}\n\n
";
heads = StringJoin[{ToCCodeString[CInclude[wflibPath]], declare}];
returnCStatement = {CStatement[CCall["MArgument_setInteger", {"Res", "0"}]], CStatement[CReturn["LIBRARY_NO_ERROR"]]};


LibraryLinkWrapperCCodeGenerate[declarationString_, libraryFunctionName_, codeSnippet_ : ""] :=
    Module[{declarationStr,externalFunctionCDecleration, headAndArgList, externalFunctionName, argsDeclarationList, callExternalCStatement},
      declarationStr=StringReplace[declarationString,"[]"..->"[]"];
      externalFunctionCDecleration = StringReplace[StringReplace["extern \"C\"{ " <> declarationStr <> ";}\n\n", "complex" -> "mcomplex"],"int"->"mint"];
      (*take cares of mcomplex type and mint type*)
      headAndArgList = StringTrim /@ StringSplit[StringReplace[declarationStr, {"void " -> ",", "(" -> ", ", ")" -> ""}], ","];
      (* seperate the external function head, and the arguments declarations*)
      externalFunctionName = headAndArgList[[1]];
      argsDeclarationList = Rest@headAndArgList;
      callExternalCStatement =
          CStatement[
            CCall[externalFunctionName,
              StringReplace[#, {"int* " :> "&", "double* " :> "&",
                "complex* " :> "&", "int" ~~ x___ ~~ "[]" :> StringTrim@x,
                "double" ~~ x___ ~~ "[]" :> StringTrim@x,
                "complex" ~~ x___ ~~ "[]" :> StringTrim@x,
                "char" ~~ x___ ~~ "[" ~~ y___ ~~ "]" :> StringTrim@x}] & /@
                  argsDeclarationList]];
      StringJoin[{
        heads,
        externalFunctionCDecleration,
        ToCCodeString@CFunction["EXTERN_C DLLEXPORT int", libraryFunctionName, {{"WolframLibraryData", "libData"}, {"mint", "Argc"}, {"MArgument*", "Args"}, {"MArgument", "Res"}},
          {
            CComment["receive arguments from Mathematica side", {"\n", "\n"}],
            getAllArguments[argsDeclarationList],
            CComment["call external function", {"\n", "\n"}],
            callExternalCStatement,
            CComment["code snippet inserted", {"\n", "\n"}],
            codeSnippet,
            CComment["clean the temperoray MTensor", {"\n", "\n"}],
            disownAllArguments[argsDeclarationList],
            CComment["library function return", {"\n", "\n"}],
            returnCStatement
          }]
      }]

    ]


CompileFortranLibraryLink[libraryLinkWrapperSourcePath_, fortranSourcePath_List, fOptions_ : "", cOptions_ : ""] := Module[{currentDir, tempDir, copyToTemp, getFileName, fName, cName, msg, shellCommand},
(*===create temp dir and save current dir=======*)
  currentDir = Directory[];(*save current directory*)
  tempDir = CreateDirectory[];(*create temp dir*)

  shellCommand[comd_] := Import["!" <> comd <> " 2>&1", "Text"];(*run shell command*)

  copyToTemp[file_] := CopyFile[file, FileNameJoin[{tempDir, StringJoin[{FileBaseName[file], ".", FileExtension[file]}]}]];
  getFileName[file_] := StringJoin[{FileBaseName[file], ".", FileExtension[file]}];

  (*===file names, base name with extensiion====*)

  fName = getFileName /@ fortranSourcePath;(*fortran file names*)
  cName = getFileName@libraryLinkWrapperSourcePath;(*c file name*)

  (*===copy all file needed to tmp directory,and cd to temp dir===*)

  copyToTemp[libraryLinkWrapperSourcePath];(*copy the wrapper file to temp*)
  copyToTemp /@ fortranSourcePath;(*copy fortran files to temp*)
  SetDirectory[tempDir];(* change to temp dir *)

  (*======compile fortran source files==========*)
  msg = shellCommand[OptionValue[$FortranLinkFCompiler,"Path"] <> " -fPIC -c -r8" <> fOptions <> " " <> StringJoin[(StringJoin[" ", #]& /@ fName)]];
  If[msg!="",Print[msg];Abort[]];
  (*======compile wrapper file==================*)
  msg = shellCommand[OptionValue[$FortranLinkCCompiler,"Path"] <> " -fPIC -c " <> cOptions <> " " <> cName];
  If[msg!="",Print[msg];Abort[]];
  (*======link object files to get dynamic lib===*)
  msg =Switch[$OperatingSystem,
  "Unix",shellCommand[OptionValue[$FortranLinkFCompiler,"Path"] <> " -fPIC -lstdc++ -shared *.o -o " <> FileBaseName[cName] <> ".dylib"],
  "MacOSX",shellCommand[OptionValue[$FortranLinkFCompiler,"Path"] <> " -fPIC -lstdc++ -dynamiclib *.o -o " <> FileBaseName[cName] <> ".dylib"],
  _,Print["System not supported"];Abort[];
  ];
  If[msg!="",Print[msg];Abort[]];
  (*===change back to original dir and return the dynamic library path=========*)
  SetDirectory[currentDir];
  (*===test whether successed====*)
  If[
    FileExistsQ[FileNameJoin[{tempDir, FileBaseName[cName] <> ".dylib"}]],
    FileNameJoin[{tempDir, FileBaseName[cName] <> ".dylib"}],
    Print[tempDir];$Failed
  ]
]

(*
Create a dynamic file from the wrapper and the source code of the fortran files.
Input:
libraryLinkWrapperSourcePath: the path to the wrapper
fortranSourcePath: a list of path to fortran source files.
fOptions: the option for compiling with fortran source fields.
cOptions: the compile options for compiling with the cpp wrapper file.

Return:

The library return the path to the dynamic library file.

Example:

CompileFortranLibraryLink["/Users/xslittlegrass/Downloads/temp/temp_\
code/calculate_eigen_states/LibraryLink_calAtomicStateEnergy_wrapper.\
cpp", {"/Users/xslittlegrass/Downloads/temp/temp_code/calculate_eigen_\
states/calAtomicStateEnergy.f90",
  "/Users/xslittlegrass/Downloads/temp/temp_code/calculate_eigen_\
states/rst1.f"}]

*)



CreateFortranLibrary[fortranFiles_List, declaration_String, libraryFunctionName_String, codeSnippet_ : ""] := Module[{tmp, wrapperPath, wrapperSource, str},

(*======create temp file as wrapper source file========*)
  tmp = CreateTemporary[];
  wrapperPath = RenameFile[tmp, tmp <> ".cpp"];

  (*======generate the wrapper source code========*)
  wrapperSource = LibraryLinkWrapperCCodeGenerate[declaration, libraryFunctionName,codeSnippet];
  (*======write wrapper source to a temp file=====*)
  str = OpenWrite[wrapperPath];
  WriteString[str, wrapperSource];
  Close[str];
  (*===\[Equal]compile the wrapper and make library file======*)
  CompileFortranLibraryLink[wrapperPath, fortranFiles]
]
(*
generate the dynamic library file from a fortran subroutine.
It generate the wrapper and then compile the wrapper and the fortran files to generate the library file.
Input:

fortranFiles: The fortran files that associated with the fortran subrourtine.
declaration: The declaration for the fortran subroutine, in c style.
libraryFunctionName: the library function name.

Example:
path = fortranSubroutineLink[{"/Users/xslittlegrass/Downloads/test11/\
rst1.f", "/Users/xslittlegrass/Downloads/test11/pseudo_pot_He.f90",
   "/Users/xslittlegrass/Downloads/test11/calAtomicStateEnergy.f90"},
  "void calatomicstateenergy(char atomName[], int* nState, int* nr, \
int* lwant, double eigenEnergy[], double eigenState[])", "myfun"]
f=LibraryFunctionLoad[path,"myfun",{"UTF8String",Integer,Integer,Integer,{Real,1,"Shared"},{Real,2,"Shared"}},Integer];
atom="He";nState=4;lwant=1;nr=1000;
eigenEnergy=Developer`ToPackedArray@Table[0.,{nState}];
eigenState=Developer`ToPackedArray@Table[0.,{nState},{nr}];
f[atom,nState,nr,lwant,eigenEnergy,eigenState]
*)


CreateFortranLibrary[fortranSrc_String, declaration_String, libraryFunctionName_String,codeSnippet_ : ""] := Module[{tmp, filePath},
  (* for the case when the first argument is a source code*)

  tmp=CreateTemporary[];
  filePath = RenameFile[tmp, tmp <> ".f90"];
  Export[filePath,fortranSrc,"String"];

  CreateFortranLibrary[{filePath}, declaration, libraryFunctionName,codeSnippet]

]



libraryLinkArguments[declarationStr_] :=
    (*parse declaration string to generate LibraryLink arguments*)
    Module[{argsDeclarationList, arg, rules,dim},

      dim[str_] := StringCount[str, "[]"];

      argsDeclarationList = Rest[StringTrim /@ StringSplit[StringReplace[declarationStr, {"void " -> ",", "(" -> ", ", ")" -> ""}], ","]];

      rules = {
        _?intStarQ :> Integer,
        _?doubleStarQ :> Real,
        _?complexStarQ :> Complex,
        arg_?intArrayQ :> {Integer, dim[arg],"Shared"},
        arg_?doubleArrayQ :> {Real, dim[arg],"Shared"},
        arg_?complexArrayQ :> {Complex, dim[arg],"Shared"},
        arg_?charArrayQ :> "UTF8String"
      };

      Replace[#, rules] & /@ argsDeclarationList

    ]



LinkFortranSubroutine[fortranSrc_String,declaration_String,codeSnippet_:""] := Module[{lib,libArgs,libraryFunctionName},


  libraryFunctionName = "LL"<>StringTrim@First@StringCases[declaration, Shortest["void " ~~ x__ ~~ "("] :> x];
  (* name for library function *)
  lib=CreateFortranLibrary[fortranSrc, declaration, libraryFunctionName,codeSnippet];
  libArgs=libraryLinkArguments[declaration];
  LibraryFunctionLoad[lib,libraryFunctionName,libArgs,Integer]

]


End[]


EndPackage[]
