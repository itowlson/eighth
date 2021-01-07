module WatFunction

open WasmInstruction
open WasmTypes

type WatFunction = {
    Name: string
    Parameters: (string * WasmType) list
    ResultTypes: WasmType list
    Locals: (string * WasmType) list
    Instructions: WasmInstruction list
    Export: string option
}
