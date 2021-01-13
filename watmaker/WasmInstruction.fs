module WasmInstruction

open WasmTypes

type LocalIndex =
    | LocalId of string
    | LocalIndex of uint
type GlobalIndex =
    | GlobalId of string

type FuncIndex =
    | FuncId of string
    | FuncIndex of uint

type WasmInstruction =
    | LocalGet of LocalIndex
    | LocalSet of LocalIndex
    | LocalTee of LocalIndex
    | GlobalGet of GlobalIndex
    | GlobalSet of GlobalIndex
    | I32Const of int
    | I32Store
    | I32Load8u
    | I32Store8
    | I32GreaterThanS
    | I32LessThanS
    | Call of FuncIndex
    | I32Add
    | I32Sub
    | I32Mul
    | I32DivS
    | I32RemS
    | Block of WasmType list * WasmType list
    | Loop of WasmType list * WasmType list
    | If of WasmType list * WasmType list
    | Else
    | End
    | BreakIf of int
    | Break of int
    | Drop
