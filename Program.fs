open System

open Result
open Ast
open Parser

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
    let text = System.IO.File.ReadAllText("./testdata/prog2.e")
    match parseefunc text with
    | Ok(m) -> printfn "%A" m; 0
    | Err(e) -> printfn "OH NO!!! %s" e; 1
