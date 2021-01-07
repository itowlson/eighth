module WatFunction

open WasmInstruction
open WasmTypes

type WatFunction = {
    Name: string
    Parameters: (string * WasmType) list
    ResultTypes: WasmType list
    Instructions: WasmInstruction list
    Export: string option
}
