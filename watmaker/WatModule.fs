module WatModule

open WatImport
open WatFunction
open WatData

open System.Text

type WatModule = {
    Imports: WatImport list
    Functions: WatFunction list
    Data: WatData list
}
