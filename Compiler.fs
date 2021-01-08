module Compiler

open Ast

open WasmTypes
open WasmInstruction
open WatImport
open WatFunction
open WatModule

let etemp1 = LocalId("$etemp1")
let etemp2 = LocalId("$etemp2")

let funcId ename = FuncId(sprintf "$%s" ename)

let compileRef (s: string) =
    if s.StartsWith("$") then
        [WasmInstruction.LocalGet(LocalId(s))]
    else
        [WasmInstruction.Call(funcId(s))]

let compileInstruction (EInstruction instruction) =
    match instruction with
    | "+"     -> [WasmInstruction.I32Add]
    | "-"     -> [WasmInstruction.I32Sub]
    | "*"     -> [WasmInstruction.I32Mul]
    | "dup"   -> [WasmInstruction.LocalTee(etemp1); WasmInstruction.LocalGet(etemp1)]
    | "swap"  -> [WasmInstruction.LocalSet(etemp1); WasmInstruction.LocalSet(etemp2); WasmInstruction.LocalGet(etemp1); WasmInstruction.LocalGet(etemp2)]
    | _       -> compileRef instruction

let compileInstructions (instructions: EInstruction list) =
    List.collect compileInstruction instructions

let rec flattenStruct (structs: EStruct list) estruct =
    estruct.Fields |> List.collect (fun field ->
        match structs |> List.tryFind (fun s -> (TypeName s.Name) = field.FieldType) with
        | Some(embstr) -> flattenStruct structs embstr
        | None         -> [I32]
    )

let lltypes (structs: EStruct list) (TypeName ty) =
    match structs |> List.tryFind (fun s -> s.Name = ty) with
    | Some(typedef) -> flattenStruct structs typedef
    | None -> [I32]

let rec flattenParamStruct (structs: EStruct list) prefix estruct =
    estruct.Fields |> List.collect (fun field ->
        match structs |> List.tryFind (fun s -> (TypeName s.Name) = field.FieldType) with
        | Some(embstr) -> flattenParamStruct structs (sprintf "%s.%s" prefix field.Name) embstr
        | None         -> [(sprintf "%s.%s" prefix field.Name, I32)]
    )

let llparameters (structs: EStruct list) argIndex (TypeName ty) =
    match structs |> List.tryFind (fun s -> s.Name = ty) with
    | Some(typedef) -> flattenParamStruct structs (sprintf "$%d" argIndex) typedef
    | None -> [(sprintf "$%d" argIndex, I32)]

let compileFunc structs (func: EFunc) =
    {
        WatFunction.Name = sprintf "$%s" func.Name
        Parameters = func.Inputs |> List.mapi (llparameters structs) |> List.concat
        ResultTypes = func.Outputs |> List.collect (lltypes structs)
        Locals = [("$etemp1", I32); ("$etemp2", I32)]
        Instructions = func.Instructions |> compileInstructions
        Export = Some(func.Name)
    }

let compileImport structs (import: EImport) =
    {
        WatImport.Source = import.SourceModule
        Name = import.Name
        ParameterTypes = import.Inputs |> List.collect (lltypes structs)
        ResultTypes = import.Outputs |> List.collect (lltypes structs)
    }

let partition syntaxItems =
    let asFunc =
        function
        | Func(x) -> Some(x)
        | _       -> None
    let asStruct =
        function
        | Struct(x) -> Some(x)
        | _         -> None
    let asImport =
        function
        | Import(x) -> Some(x)
        | _         -> None
    let funcs = syntaxItems |> List.choose asFunc
    let structs = syntaxItems |> List.choose asStruct
    let imports = syntaxItems |> List.choose asImport
    (funcs, structs, imports)

let compileModule syntaxItems =
    let (funcs, structs, imports) = partition syntaxItems
    {
        Functions = funcs |> List.map (compileFunc structs)
        Imports = imports |> List.map (compileImport structs)
        Data = []
    }