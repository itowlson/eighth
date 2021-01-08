module Ast

type TypeName = TypeName of string

type Parameter = {
    Name: string
    Type: TypeName
}

type Instruction = {
    OpCode: string
    Arg: string option
}

type Func = {
    Name: string
    Parameters: Parameter list
    Result: TypeName option
    Instructions: Instruction list
}

type Module = {
    Functions: Func list
}

type EField = {
    Name: string
    FieldType: TypeName
}
type EStruct = {
    Name: string
    Fields: EField list
}

type EInstruction = EInstruction of string

type EFunc = {
    Name: string
    Inputs: TypeName list
    Outputs: TypeName list
    Instructions: EInstruction list
}

type EImport = {
    SourceModule: string
    Name: string
    Inputs: TypeName list
    Outputs: TypeName list
}
type ESyntaxItem =
| Func of EFunc
| Struct of EStruct
| Import of EImport
