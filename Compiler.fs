module Compiler

open Ast

open WasmTypes
open WasmInstruction
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

// let setupStack (inputs: TypeName list) =
//     inputs |> List.mapi (fun index _ -> WasmInstruction.LocalGet(LocalId(sprintf "$%d" index))) |> List.rev

let lltypes (structs: EStruct list) (TypeName ty) =
    match structs |> List.tryFind (fun s -> s.Name = ty) with
    | Some(typedef) -> typedef.Fields |> List.map (fun field -> I32)  // TODO: nested stricts
    | None -> [I32]

let llparameters (structs: EStruct list) argIndex (TypeName ty) =
    match structs |> List.tryFind (fun s -> s.Name = ty) with
    | Some(typedef) -> typedef.Fields |> List.map (fun field -> (sprintf "$%d.%s" argIndex field.Name, I32))  // TODO: nested stricts
    | None -> [(sprintf "$%d" argIndex, I32)]

let compileFunc structs (func: EFunc) =
    {
        WatFunction.Name = sprintf "$%s" func.Name
        Parameters = func.Inputs |> List.mapi (llparameters structs) |> List.concat
        ResultTypes = func.Outputs |> List.map (lltypes structs) |> List.concat
        Locals = [("$etemp1", I32); ("$etemp2", I32)]
        Instructions = func.Instructions |> compileInstructions
        Export = Some(func.Name)
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
    let funcs = syntaxItems |> List.choose asFunc
    let structs = syntaxItems |> List.choose asStruct
    (funcs, structs)

let compileModule syntaxItems =
    let (funcs, structs) = partition syntaxItems
    {
        Functions = funcs |> List.map (compileFunc structs)
        Imports = []
        Data = []
    }