module Compiler

open Ast

open WasmTypes
open WasmInstruction
open WatImport
open WatFunction
open WatData
open WatModule

type BlockKind =
| NoBlock
| Loop

type BlockedInstruction =
| BInstruction of string
| Block of BlockKind * BlockedInstruction list

let etemp1 = LocalId("$etemp1")
let etemp2 = LocalId("$etemp2")
let lc = LocalId("$lc")  // will need to be able to nest these
let lstash = LocalId("$lstash")  // will need to be able to nest these

let funcId ename = FuncId(sprintf "$%s" ename)

let (|NumLiteral|_|) (s: string) =
    match System.Int32.TryParse(s) with
    | (true, n) -> Some n
    | _         -> None

let (|ConstRef|_|) (consts: EConst list) s =
    consts |> List.tryFind (fun c -> c.Name = s)

let (|LocalRef|_|) (s: string) =
    if s.StartsWith("$") then Some(s) else None

let compileRef consts s =
    match s with
    | NumLiteral n      -> [WasmInstruction.I32Const(n)]
    | ConstRef consts c -> [WasmInstruction.I32Const(c.Value)]
    | LocalRef r        -> [WasmInstruction.LocalGet(LocalId(r))]
    | _                 -> [WasmInstruction.Call(funcId(s))]

let loopPrologue = [
            WasmInstruction.LocalSet(lstash)
            WasmInstruction.LocalSet(lc)
            WasmInstruction.Block( (* how to detect block type? *) )
            WasmInstruction.Loop( (* how to detect block type? *) )
        ]

let loopEpilogue = [
            WasmInstruction.LocalGet(lc)
            WasmInstruction.I32Const(1)
            WasmInstruction.I32Add
            WasmInstruction.LocalTee(lc)
            WasmInstruction.LocalGet(lstash)
            WasmInstruction.I32GreaterThanS
            WasmInstruction.BreakIf(1)
            WasmInstruction.Break(0)
            WasmInstruction.End
            WasmInstruction.End
        ]

let rec compileInstruction consts instruction =
    match instruction with
    | BInstruction name ->
        match name with
        | "+"     -> [WasmInstruction.I32Add]
        | "-"     -> [WasmInstruction.I32Sub]
        | "*"     -> [WasmInstruction.I32Mul]
        | "store" -> [WasmInstruction.I32Store]
        | "drop"  -> [WasmInstruction.Drop]
        | "dup"   -> [WasmInstruction.LocalTee(etemp1); WasmInstruction.LocalGet(etemp1)]
        | "swap"  -> [WasmInstruction.LocalSet(etemp1); WasmInstruction.LocalSet(etemp2); WasmInstruction.LocalGet(etemp1); WasmInstruction.LocalGet(etemp2)]
        | "index" -> [WasmInstruction.LocalGet(lc)]
        | _       -> compileRef consts name
    | Block(Loop, body) -> loopPrologue @ (body |> List.collect (compileInstruction consts)) @ loopEpilogue
    | Block(NoBlock, body) -> raise (System.InvalidOperationException("not expecting NoBlock at top level"))

let asInstruction =
    function
    | EInstruction s       -> Some s
    | EInstruction.Comment -> None

let fst3 (a, _, _) = a

let emblocken instructions =
    let rec emblockenOnto (acc: BlockedInstruction list) (instructions: string list) =
        match instructions with
        | [] -> (acc, [], NoBlock)
        | "do" :: rest -> let (blockBody, rest', kind) = emblockenOnto [] rest
                          let block = Block(kind, (List.rev blockBody))
                          emblockenOnto (block :: acc) rest'
        | "loop" :: rest -> (acc, rest, BlockKind.Loop)
        | instr :: rest -> emblockenOnto ((BInstruction instr) :: acc) rest
    instructions |> emblockenOnto [] |> fst3 |> List.rev

let compileInstructions consts (instructions: EInstruction list) =
    let actualInstructions = instructions |> List.choose asInstruction |> emblocken
    List.collect (compileInstruction consts) actualInstructions

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

let compileFunc structs consts (func: EFunc) =
    {
        WatFunction.Name = sprintf "$%s" func.Name
        Parameters = func.Inputs |> List.mapi (llparameters structs) |> List.concat
        ResultTypes = func.Outputs |> List.collect (lltypes structs)
        Locals = [("$etemp1", I32); ("$etemp2", I32); ("$lc", I32); ("$lstash", I32)]
        Instructions = func.Instructions |> compileInstructions consts
        Export = Some(func.Name)
    }

let compileImport structs (import: EImport) =
    {
        WatImport.Source = import.SourceModule
        Name = import.Name
        ParameterTypes = import.Inputs |> List.collect (lltypes structs)
        ResultTypes = import.Outputs |> List.collect (lltypes structs)
    }

let compileDataContent =
    function
    | Text(s) -> StringContent s
    | Raw(by) -> RawContent by

let compileData (data: EData) =
    {
        Offset = data.Address
        Content = compileDataContent data.Data
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
    let asConst =
        function
        | Const(x) -> Some(x)
        | _        -> None
    let asData =
        function
        | Data(x)  -> Some(x)
        | _        -> None
    let funcs = syntaxItems |> List.choose asFunc
    let structs = syntaxItems |> List.choose asStruct
    let imports = syntaxItems |> List.choose asImport
    let consts = syntaxItems |> List.choose asConst
    let datas = syntaxItems |> List.choose asData
    (funcs, structs, imports, consts, datas)

let compileModule syntaxItems =
    let (funcs, structs, imports, consts, datas) = partition syntaxItems
    {
        Functions = funcs |> List.map (compileFunc structs consts)
        Imports = imports |> List.map (compileImport structs)
        Data = datas |> List.map compileData
    }