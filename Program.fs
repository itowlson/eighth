open System

open Result
open Ast
open Parser
open Compiler
open WatWriter

(*
We want the output to be a Wasm func:

(func $blah
  (export "blah")opt
  (param $name ty)*
  (result ty)opt
  instr*
)
*)

[<EntryPoint>]
let main argv =
    let text = System.IO.File.ReadAllText("./testdata/prog4a.e")
    match parseefuncs text with
    | Ok(m) ->  printfn "%A" m
                let wat = compileModule m
                let writer = WatWriter.New()
                writeModule writer wat
                printfn "%s" (WatWriter.text writer)
                System.IO.Directory.CreateDirectory("out") |> ignore
                System.IO.File.WriteAllText("out/gen.wat", (WatWriter.text writer))
                0
    | Err(e) -> printfn "OH NO!!! %s" e; 1
