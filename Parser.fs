module Parser

open Result
open Ast
open FParsec

let makeParameter name typeName = {
    Name = name
    Type = typeName
}

let makeFunc name parameters result instructions = {
    Name = name
    Parameters = parameters
    Result = result
    Instructions = instructions // List.map (fun s -> {OpCode = s; Arg = None}) instructions
}

let makeInstruction opcode arg = {
    OpCode = opcode
    Arg = arg
}

let makeModule funcs = {
    Functions = funcs
}

let makeEFunc name (inputs, outputs) instructions = {
    Name = name
    Inputs = inputs
    Outputs = outputs
    Instructions = instructions
}

let ws p = spaces >>. p .>> spaces
let lit s = pstring s
let litw s = ws (lit s)
// let sp = many (pchar ' ')
let sp1 = many1 (pchar ' ')

// TODO: really there should be mandatory spacing not just ignoring whitespace if present
let spaced2 p1 p2 f = pipe2 (ws p1) (ws p2) f
let spaced3 p1 p2 p3 f = pipe3 (ws p1) (ws p2) (ws p3) f
let spaced4 p1 p2 p3 p4 f = pipe4 (ws p1) (ws p2) (ws p3) (ws p4) f

let between before after s2 p = before >>. p .>> after
let betweenStrings s1 s2 p = litw s1 >>. p .>> litw s2
let bracketed p = betweenStrings "(" ")" p
let braced p = betweenStrings "{" "}" p
let sqbracketed p = betweenStrings "[" "]" p

let isNameStart c = isAsciiLetter c || (c = '$')
let nameOptions = IdentifierOptions(isAsciiIdStart = isNameStart)
let typeNameOptions = IdentifierOptions(isAsciiIdStart = isAsciiLetter)

let name = (identifier nameOptions)
let typeName = ((identifier typeNameOptions) |>> TypeName)

let param = bracketed (spaced2 name typeName makeParameter)
let result = litw "->" >>. typeName

let instruction = pipe2 (regex "[._a-zA-Z0-9]+") (opt (sp1 >>. regex "[a-zA-Z0-9$]+")) makeInstruction
// let instructions = braced (regex "[^}]*") |>> (fun (s: string) -> s.Split("\n") |> List.ofArray) // (sepBy (sp >>. instruction .>> sp) newline)
// let instructions = spaces >>. pchar '{' >>. spaces >>. (sepBy (regex "[a-zA-Z0-9$._]+") spaces) .>> spaces .>> pchar '}' .>> spaces
let instructions = braced (sepEndBy instruction spaces1)

let funcbody = spaced4 name (many param) (opt result) instructions makeFunc
let func = bracketed (litw "func" >>. funcbody)

let modulebody = (many func) |>> makeModule
let emodule = bracketed (litw "module" >>. modulebody)

let types = sqbracketed (sepEndBy typeName spaces)
let signature = types .>> litw "->" .>>. types 

let isEInstructionChar c = not(System.Char.IsWhiteSpace(c) || (c = '}'))
let einstruction = many1Satisfy isEInstructionChar |>> EInstruction
let einstructions = braced (sepEndBy einstruction spaces)

let efuncbody = pipe3 name signature einstructions makeEFunc
let efunc = litw "func" >>. efuncbody
let efuncs = sepEndBy efunc spaces

let parseModule str =
    match run emodule str with
    | Success(result, _, _) -> Result.Ok(result)
    | Failure(errorMsg, _, _) -> Err(errorMsg)

let parseefuncs str =
    match run efuncs str with
    | Success(result, _, _) -> Result.Ok(result)
    | Failure(errorMsg, _, _) -> Err(errorMsg)

let testtest str =
    match run instructions str with
    | Success(result, _, _) -> Result.Ok(result)
    | Failure(errorMsg, _, _) -> Err(errorMsg)
