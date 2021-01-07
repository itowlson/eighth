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

type EInstruction = EInstruction of string

type EFunc = {
    Name: string
    Inputs: TypeName list
    Outputs: TypeName list
    Instructions: EInstruction list
}

