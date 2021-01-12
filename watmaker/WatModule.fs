module WatModule

open WatImport
open WatFunction
open WatData
open WatGlobal

open System.Text

type WatModule = {
    Imports: WatImport list
    Functions: WatFunction list
    Data: WatData list
    Globals: WatGlobal list
}
