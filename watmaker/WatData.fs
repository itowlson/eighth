module WatData

type DataContent =
    | StringContent of string
    | RawContent of byte[]

type WatData = {
    Offset: int
    Content: DataContent
}
