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
            let expected_str = fp_expected.ToExpr(0)
            let got_str = fp_got.ToExpr(0)
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
        let fp_expected = FPCore([FPSymbol("a1")], [], Symbol(FPSymbol("a1")))
        ParserTest xl_expr fp_expected

    [<Test>]
    member this.SUMExpr1() =
        // should be: (FPCore (a1 a2 a3) (+ (+ a1 a2) a3))
        let xl_expr = "=SUM(A1:A3)"
        let fp_expected =
            let a1 = FPSymbol("a1")
            let a2 = FPSymbol("a2")
            let a3 = FPSymbol("a3")
            FPCore(
                [a1; a2; a3],
                [],
                FPExpr.Operation(
                    FPOperation.MathOperation(
                        FPMathOperation.Plus,
                        [FPExpr.Operation(
                            FPOperation.MathOperation(
                                FPMathOperation.Plus,
                                [Symbol(a1);
                                 Symbol(a2)]
                            )
                         );
                         Symbol(a3)]
                    )
                )
            )
        ParserTest xl_expr fp_expected

    [<Test>]
    member this.PrecedenceTest() =
        // should be:
        let xl_expr = "=2*2+2"
        let fp_expected =
            FPCore(
                [],
                [],
                Operation(
                    MathOperation(
                        Plus,
                        [Operation(
                            MathOperation(
                                Multiply,
                                [Num(FPNum(2.0));
                                 Num(FPNum(2.0))]
                            )
                        );
                        Num(FPNum(2.0))]
                    )
                )
            )
        ParserTest xl_expr fp_expected