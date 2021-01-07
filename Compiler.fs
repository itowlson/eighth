module Compiler

open Ast

open WasmTypes
open WasmInstruction
open WatFunction
open WatModule

let compileInstruction (EInstruction instruction) =
    match instruction with
    | "+"     -> [WasmInstruction.I32Add]
    | "*"     -> [WasmInstruction.I32Mul]
    | "dup"   -> [WasmInstruction.Dup]
    | "swap"  -> [WasmInstruction.Swap]
    | _       -> raise (System.ArgumentException("instruction"))
    

let compileInstructions (instructions: EInstruction list) =
    List.collect compileInstruction instructions

let compileFunc (func: EFunc) =
    {
        WatFunction.Name = sprintf "$%s" func.Name
        Parameters = func.Inputs |> List.mapi (fun index ty -> (sprintf "$%d" index, I32))
        ResultTypes = func.Outputs |> List.map (fun ty -> I32)
        Instructions = func.Instructions |> compileInstructions
        Export = None
    }

let compileModule func =
    {
        Functions = [compileFunc func]
        Imports = []
        Data = []
    }