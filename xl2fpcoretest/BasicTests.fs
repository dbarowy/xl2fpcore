namespace xl2fpcoretest

open NUnit.Framework
open FParsec
open FPCoreAST

exception ParseException of string

[<TestFixture>]
type BasicTests () =

    // handy shortcuts
    let XLParser = Grammar.Formula
    let EmptyEnvironment = AST.Env("", "", "")

    let GetAST str =
        let ast = 
            match runParserOnString XLParser EmptyEnvironment "" str with
                | Success(result, _, _)     -> result
                | Failure(errorMsg, _, _)   -> raise (ParseException errorMsg)
        ast

    let ParserTest(xl_expr: string)(fp_expected: FPCore) : unit =
        try
            let ast = GetAST xl_expr
            let fp_got = XL2FPCore.FormulaToFPCore ast
            Assert.AreEqual(fp_expected, fp_got)
        with
        | :? ParseException -> Assert.Fail()

    [<Test>]
    member this.SimpleConstantExpr() =
        let xl_expr = "=1"
        let fp_expected = FPCore([], [], FPExpr.Num(FPNum(1.0)))
        ParserTest xl_expr fp_expected

    [<Test>]
    member this.SimpleReferenceExpr() =
        let xl_expr = "=A1"
        let fp_expected = FPCore([], [], FPExpr.Symbol(FPSymbol("a1")))
        ParserTest xl_expr fp_expected