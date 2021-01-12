module WatGlobal

open WasmTypes

type WatGlobal = {
    GlobalName: string
    GlobalType: WasmType
    IsMutable: bool
    InitialValue: int
}
