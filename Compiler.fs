module Compiler

open Ast

open WasmTypes
open WasmInstruction
open WatImport
open WatFunction
open WatData
open WatGlobal
open WatModule

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

type Definitions = {
    Consts: EConst list
    Structs: EStruct list
    Globals: EGlobal list
}

type BlockedInstruction =
| BInstruction of string
| BLoop of BlockedInstruction list
| BIf of BlockedInstruction list * BlockedInstruction list option

let etemp1 = LocalId("$etemp1")
let etemp2 = LocalId("$etemp2")
let lc = LocalId("$lc")  // will need to be able to nest these
let lstash = LocalId("$lstash")  // will need to be able to nest these

let funcId ename = FuncId(sprintf "$%s" ename)
let globalId ename = GlobalId(sprintf "$%s" ename)

let (|NumLiteral|_|) (s: string) =
    match System.Int32.TryParse(s) with
    | (true, n) -> Some n
    | _         -> None

let (|ConstRef|_|) defns s =
    (defns.Consts) |> List.tryFind (fun c -> c.Name = s)

let (|LocalRef|_|) (s: string) =
    if s.StartsWith("$") then Some(s) else None

let (|GlobalRef|_|) defns (s: string) =
    defns.Globals |> List.tryFind (fun g -> g.Name = s)

let (|GlobalSet|_|) defns (s: string) =
    defns.Globals |> List.tryFind (fun g -> sprintf ">%s" g.Name = s)

let compileRef defns localtypes s =
    match s with
    | NumLiteral n      -> [WasmInstruction.I32Const(n)]
    | ConstRef defns c  -> [WasmInstruction.I32Const(c.Value)]
    | LocalRef r        -> match List.tryFind (fun (n, _) -> n = r) localtypes with
                           | None -> [WasmInstruction.LocalGet(LocalId(r))]  // this probably shouldn't happen
                           | Some(_, TypeName ty) ->
                                match (defns.Structs) |> List.tryFind (fun es -> es.Name = ty) with
                                | None -> [WasmInstruction.LocalGet(LocalId(r))]  // not a custom type
                                | Some(typedef) -> let members = flattenParamStruct (defns.Structs) r typedef
                                                   members |> List.map (fun (name, _) -> WasmInstruction.LocalGet(LocalId(name)))
    | GlobalRef defns g -> [WasmInstruction.GlobalGet(globalId(g.Name))]
    | GlobalSet defns g -> [WasmInstruction.GlobalSet(globalId(g.Name))]
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

let rec compileInstruction defns localtypes instruction =
    match instruction with
    | BInstruction name ->
        match name with
        | "+"     -> [WasmInstruction.I32Add]
        | "-"     -> [WasmInstruction.I32Sub]
        | "*"     -> [WasmInstruction.I32Mul]
        | "/"     -> [WasmInstruction.I32DivS]
        | "%"     -> [WasmInstruction.I32RemS]
        | "<"     -> [WasmInstruction.I32LessThanS]
        | ">"     -> [WasmInstruction.I32GreaterThanS]
        | "store" -> [WasmInstruction.I32Store]
        | "load8u" -> [WasmInstruction.I32Load8u]
        | "store8" -> [WasmInstruction.I32Store8]
        | "drop"  -> [WasmInstruction.Drop]
        | "dup"   -> [WasmInstruction.LocalTee(etemp1); WasmInstruction.LocalGet(etemp1)]
        | "swap"  -> [WasmInstruction.LocalSet(etemp1); WasmInstruction.LocalSet(etemp2); WasmInstruction.LocalGet(etemp1); WasmInstruction.LocalGet(etemp2)]
        | "index" -> [WasmInstruction.LocalGet(lc)]
        | _       -> compileRef defns localtypes name
    | BLoop body -> loopPrologue @ (body |> List.collect (compileInstruction defns localtypes)) @ loopEpilogue
    | BIf (ifbody, Some elsebody) ->
        [WasmInstruction.If( (* how to detect block type *) )] @
        (ifbody |> List.collect (compileInstruction defns localtypes)) @
        [WasmInstruction.Else] @
        (elsebody |> List.collect (compileInstruction defns localtypes)) @
        [WasmInstruction.End]
    | BIf (ifbody, None) ->
        [WasmInstruction.If( (* how to detect block type *) )] @
        (ifbody |> List.collect (compileInstruction defns localtypes)) @
        [WasmInstruction.End]

let asInstruction =
    function
    | EInstruction s       -> Some s
    | EInstruction.Comment -> None

let fst3 (a, _, _) = a

let emblocken instructions =
    let rec emblockenOnto (acc: BlockedInstruction list) (instructions: string list) =
        match instructions with
        | [] -> (acc, [])
        | "do" :: rest -> let (block, rest') = emblockenOnto [] rest
                          emblockenOnto (block @ acc) rest'
        | "loop" :: rest -> ([BLoop (List.rev acc)], rest)
        | "if" :: rest -> let (block, rest') = emblockenOnto [] rest
                          emblockenOnto (block @ acc) rest'
        | "else" :: rest -> let ifBody = List.rev acc
                            let (elseBlock, rest') = emblockenOnto [] rest
                            match elseBlock with
                            | [BIf (body, None)] -> ([BIf(ifBody, Some body)], rest')
                            | _ -> raise (System.InvalidOperationException("messed up the if-else logic"))
        | "then" :: rest -> ([BIf (List.rev acc, None)], rest)
        | instr :: rest -> emblockenOnto ((BInstruction instr) :: acc) rest
    instructions |> emblockenOnto [] |> fst |> List.rev

let compileInstructions consts localtypes (instructions: EInstruction list) =
    let actualInstructions = instructions |> List.choose asInstruction |> emblocken
    List.collect (compileInstruction consts localtypes) actualInstructions

let compileFunc defns (func: EFunc) =
    let localtypes = func.Inputs |> List.mapi (fun index ty -> ((sprintf "$%d" index), ty))
    {
        WatFunction.Name = sprintf "$%s" func.Name
        Parameters = func.Inputs |> List.mapi (llparameters (defns.Structs)) |> List.concat
        ResultTypes = func.Outputs |> List.collect (lltypes (defns.Structs))
        Locals = [("$etemp1", I32); ("$etemp2", I32); ("$lc", I32); ("$lstash", I32)]
        Instructions = func.Instructions |> compileInstructions defns localtypes
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

let compileGlobal (g: EGlobal) =
    {
        GlobalName = sprintf "$%s" g.Name
        GlobalType = I32 // g.GlobalType
        IsMutable = true
        InitialValue = g.InitialValue
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
    let asGlobal =
        function
        | Global(x) -> Some(x)
        | _         -> None
    let asData =
        function
        | Data(x)  -> Some(x)
        | _        -> None
    let funcs = syntaxItems |> List.choose asFunc
    let structs = syntaxItems |> List.choose asStruct
    let imports = syntaxItems |> List.choose asImport
    let consts = syntaxItems |> List.choose asConst
    let globals = syntaxItems |> List.choose asGlobal
    let datas = syntaxItems |> List.choose asData
    (funcs, structs, imports, consts, globals, datas)

let compileModule syntaxItems =
    let (funcs, structs, imports, consts, globals, datas) = partition syntaxItems
    let defns = { Consts = consts; Structs = structs; Globals = globals }
    {
        Functions = funcs |> List.map (compileFunc defns)
        Imports = imports |> List.map (compileImport structs)
        Data = datas |> List.map compileData
        Globals = globals |> List.map compileGlobal
    }
