module WatImport

open WasmTypes

type WatImport = {
    Source: string
    Name: string
    ParameterTypes: WasmType list
    ResultTypes: WasmType list
}
