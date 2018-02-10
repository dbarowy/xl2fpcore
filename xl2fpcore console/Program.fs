open System
open Parcel
open FParsec

printfn "Excel to FPCore Converter"
printfn "Type an Excel formula expression or 'quit'."

exception ParseException of string

let EmptyEnvironment = AST.Env("", "", "")
let XLParser = Grammar.Formula

let GetAST str =
    let ast = 
        match runParserOnString XLParser EmptyEnvironment "" str with
            | Success(result, _, _)     -> result
            | Failure(errorMsg, _, _)   -> raise (ParseException errorMsg)
    ast

let rec REPL() =
    printf "> "
    match Console.ReadLine() with
    | "quit" -> ()
    | input ->
        try
            printfn "Parsing..."
            let ast = GetAST input
            let fpstr = XL2FPCore.FormulaToFPCore ast
            printfn "%A" (fpstr.ToExpr 0)
        with
        | :? AST.IndirectAddressingNotSupportedException as ex ->
            printfn "Indirect addressing mode is not presently supported:\n%s" ex.Message
        | _ as ex ->
            printfn "Unhandled Exception: %s" ex.Message
            if (ex.InnerException <> null) then
                printfn "Inner Exception: %s" ex.InnerException.Message

        REPL()

REPL()