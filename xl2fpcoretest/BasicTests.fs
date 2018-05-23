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

    [<Test>]
    member this.SimpleConstantExpr() =
        let fp_expected = FPCore([], [], FPExpr.Num(FPNum(1.0)))

        try
            let expr = "=1"
            let ast = GetAST expr
            let fp_got = XL2FPCore.FormulaToFPCore ast
            Assert.AreEqual(fp_expected, fp_got)
        with
        | :? ParseException -> Assert.Fail()
