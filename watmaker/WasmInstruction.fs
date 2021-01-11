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
    | LocalSet of LocalIndex
    | LocalTee of LocalIndex
    | I32Const of int
    | I32Store
    | I32Load8u
    | I32Store8
    | I32GreaterThanS
    | Call of FuncIndex
    | I32Add
    | I32Sub
    | I32Mul
    | Block of unit
    | Loop of unit
    | End
    | BreakIf of int
    | Break of int
    | Drop
