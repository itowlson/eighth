module WasmInstruction

open WasmTypes

type LocalIndex =
    | LocalId of string
    | LocalIndex of uint
type FuncIndex =
    | FuncId of string
    | FuncIndex of uint

type WasmInstruction =
    | LocalGet of LocalIndex
    | I32Const of int
    | I32Store
    | Call of FuncIndex
    | Drop
