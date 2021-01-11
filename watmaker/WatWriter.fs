module WatWriter

open System.Text

open WasmTypes
open WasmInstruction
open WatImport
open WatFunction
open WatData
open WatModule

type WatWriter = {
    Builder: StringBuilder
    Indent: int
}

let New () =
    {
        Builder = new StringBuilder()
        Indent = 0
    }

let text writer =
    writer.Builder.ToString()

let indent writer =
    new string(' ', writer.Indent)

let writeLine writer text =
    writer.Builder.AppendLine(sprintf "%s%s" (indent writer) text) |> ignore

let writeLineItems writer texts =
    match texts with
    | [] -> ()
    | _ -> writeLine writer (System.String.Join(" ", texts))

let indented writer action =
    let indentedWriter = { writer with Indent = writer.Indent + 2 }
    action indentedWriter

let typeId ty =
    match ty with
    | I32 -> "i32"
    | I64 -> "i64"
    | F32 -> "f32"
    | F64 -> "f64"

let indexText =
    function
    | LocalIndex(n) -> n.ToString()
    | LocalId(s) -> s

let writeInstruction writer instruction =
    let text = 
        match instruction with
        | LocalGet(index) -> sprintf "local.get %s" (indexText index)
        | LocalSet(index) -> sprintf "local.set %s" (indexText index)
        | LocalTee(index) -> sprintf "local.tee %s" (indexText index)
        | I32Const(value) -> sprintf "i32.const %d" value
        | I32Store -> "i32.store"
        | I32Load8u -> "i32.load8_u"
        | I32Store8 -> "i32.store8"
        | I32GreaterThanS -> "i32.gt_s"
        | I32Add -> "i32.add"
        | I32Sub -> "i32.sub"
        | I32Mul -> "i32.mul"
        | Call(index) ->
            let argtext =
                match index with
                | FuncIndex(n) -> n.ToString()
                | FuncId(s) -> s
            sprintf "call %s" argtext
        | Block() -> "block (param i32) (result i32)"
        | Loop() -> "loop (param i32) (result i32)"
        | End -> "end"
        | BreakIf(idx) -> sprintf "br_if %d" idx
        | Break(idx) -> sprintf "br %d" idx
        | Drop -> "drop"
    writeLine writer text

let contentText =
    function
    | StringContent(s) -> sprintf "\"%s\"" s
    | RawContent(bs) -> raise (System.ArgumentException("oh no"))

let writeImport writer watImport =
    writeLine writer (sprintf "(import \"%s\" \"%s\"" watImport.Source watImport.Name)
    indented writer (fun writer ->
        writeLine writer (sprintf "(func $%s" watImport.Name)
        indented writer (fun writer ->
            writeLineItems writer (List.map (typeId >> sprintf "(param %s)") watImport.ParameterTypes)
            writeLineItems writer (List.map (typeId >> sprintf "(result %s)") watImport.ResultTypes)
        )
        writeLine writer ")"
    )
    writeLine writer ")"

let writeFunction writer watFunction =
    writeLine writer ("(func " + watFunction.Name)
    indented writer (fun writer ->
        match watFunction.Export with
        | Some name -> writeLine writer (sprintf """(export "%s")""" name)
        | None -> ()
        writeLineItems writer (List.map (fun (n, ty) -> sprintf "(param %s %s)" n (typeId ty)) watFunction.Parameters)
        writeLineItems writer (List.map (typeId >> sprintf "(result %s)") watFunction.ResultTypes)
        writeLineItems writer (List.map (fun (n, ty) -> sprintf "(local %s %s)" n (typeId ty)) watFunction.Locals)
        List.iter (writeInstruction writer) watFunction.Instructions
    )
    writeLine writer ")"

let writeData writer watData =
    writeLine writer (sprintf "(data (i32.const %d)" watData.Offset)
    indented writer (fun writer ->
        writeLine writer (contentText watData.Content)
    )
    writeLine writer ")"

let writeModule writer watModule =
    writeLine writer "(module"
    indented writer (fun writer ->
        writeLine writer "(memory 1)"
        writeLine writer """(export "memory" (memory 0))"""
        List.iter (writeImport writer) watModule.Imports
        List.iter (writeFunction writer) watModule.Functions
        List.iter (writeData writer) watModule.Data
    )
    writeLine writer ")"
