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

let setupStack (inputs: TypeName list) =
    inputs |> List.mapi (fun index _ -> WasmInstruction.LocalGet(LocalId(sprintf "$%d" index))) |> List.rev

let compileFunc (func: EFunc) =
    {
        WatFunction.Name = sprintf "$%s" func.Name
        Parameters = func.Inputs |> List.mapi (fun index ty -> (sprintf "$%d" index, I32))
        ResultTypes = func.Outputs |> List.map (fun ty -> I32)
        Locals = [("$etemp1", I32); ("$etemp2", I32)]
        // Instructions = (func.Inputs |> setupStack) @ (func.Instructions |> compileInstructions)
        Instructions = func.Instructions |> compileInstructions
        Export = Some(func.Name)
    }

let compileModule funcs =
    {
        Functions = funcs |> List.map compileFunc
        Imports = []
        Data = []
    }