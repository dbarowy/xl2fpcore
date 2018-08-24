namespace xl2fpcoretest

open Microsoft.VisualStudio.TestTools.UnitTesting
open FParsec
open FPCoreAST
open System
open System.Collections.Generic

exception ParseException of string

type Bindings = System.Collections.Generic.Dictionary<AST.Address*bool*bool,string>

[<TestClass>]
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

    let ParseAndTestWithBindings(xl_expr: string)(fp_expected: FPCore)(bindings: Bindings) : unit =
        try
            let pre = new List<Dictionary<string,double>>()
            let prov = [||]
            let ast = GetAST xl_expr
            let fp_got = XL2FPCore.FormulaToFPCore ast pre bindings prov
            let expected_str = fp_expected.ToExpr(0)
            let got_str = fp_got.ToExpr(0)
            Assert.AreEqual(fp_expected, fp_got)
        with
        | :? ParseException -> Assert.Fail()

    let ParseAndTest(xl_expr: string)(fp_expected: FPCore) : unit =
        try
            let ast = GetAST xl_expr
            let fp_got = XL2FPCore.FormulaToFPCoreSimple ast
            let expected_str = fp_expected.ToExpr(0)
            let got_str = fp_got.ToExpr(0)
            Assert.AreEqual(fp_expected, fp_got)
        with
        | :? ParseException -> Assert.Fail()

    let EZBindings(addrs: string list) : Bindings =
        let bindings = new Bindings()
        addrs
        |> List.iter (fun addr ->
            bindings.Add((AST.Address.FromA1String(addr.ToUpper(),"","",""), false, false), addr.ToLower())
        )
        bindings

    [<TestMethod>]
    member self.SimpleConstantExpr() =
        let xl_expr = "=1"
        let fp_expected = FPCore([], [], FPExpr.Num(1.0))
        ParseAndTest xl_expr fp_expected

    [<TestMethod>]
    member self.SimpleReferenceExpr() =
        let xl_expr = "=A1"
        let bindings = EZBindings ["a1"]
        let fp_expected = FPCore([FPSymbol("a1")], [], Symbol(FPSymbol("a1")))
        ParseAndTestWithBindings xl_expr fp_expected bindings

    [<TestMethod>]
    member self.AVERAGEExpr1() =
        // should be: (FPCore (a1 a2 a3) (/ (+ (+ a1 a2) a3)3))
        let xl_expr = "=AVERAGE(A1:A3)"
        let bindings = EZBindings ["a1"; "a2"; "a3"]
        let fp_expected =
            let a1 = FPSymbol("a1")
            let a2 = FPSymbol("a2")
            let a3 = FPSymbol("a3")
            FPCore(
                [a1; a2; a3],
                [],
                Operation(
                    MathOperation(
                        Divide,
                        [Operation(
                             MathOperation(
                                Plus,
                                [Operation(
                                    MathOperation(
                                        Plus,
                                        [Symbol(a1);
                                         Symbol(a2)]
                                    )
                                );
                                Symbol(a3)]
                             )
                        );
                        Num(3.0)]
                    )
                )
            )
        ParseAndTestWithBindings xl_expr fp_expected bindings
    
    [<TestMethod>]
    member self.MINExpr1() =
        // should be: (FPCore (a1 a2 a3) (fmin (fmin a1 a2) a3))
        let xl_expr = "=MIN(A1:A3)"
        let bindings = EZBindings ["a1"; "a2"; "a3"]
        let fp_expected =
            let a1 = FPSymbol("a1")
            let a2 = FPSymbol("a2")
            let a3 = FPSymbol("a3")
            FPCore(
                [a1; a2; a3],
                [],
                Operation(
                    MathOperation(
                        Fmin,
                        [Operation(
                            MathOperation(
                                Fmin,
                                [Symbol(a1);
                                 Symbol(a2)]
                            )
                         );
                         Symbol(a3)]
                    )
                )
            )
        ParseAndTestWithBindings xl_expr fp_expected bindings

    [<TestMethod>]
    member self.MINExpr2() =
        // should be: (FPCore () (fmin (fmin (fmin 1 2) 3) 4))
        let xl_expr = "=MIN(1,2,3,4)"
        let fp_expected =
            FPCore(
                [],
                [],
                Operation(
                    MathOperation(
                        Fmin,
                        [Operation(
                            MathOperation(
                                Fmin,
                                [Operation(
                                    MathOperation(
                                        Fmin,
                                        [Num(1.0);
                                         Num(2.0)]
                                    )
                                 );
                                 Num(3.0)]
                            )
                         );
                         Num(4.0)]
                    )
                )
            )
        ParseAndTest xl_expr fp_expected

    [<TestMethod>]
    member self.MAXExpr1() =
        // should be: (FPCore (a1 a2 a3) (fmax (fmax a1 a2) a3))
        let xl_expr = "=MAX(A1:A3)"
        let bindings = EZBindings ["a1"; "a2"; "a3"]
        let fp_expected =
            let a1 = FPSymbol("a1")
            let a2 = FPSymbol("a2")
            let a3 = FPSymbol("a3")
            FPCore(
                [a1; a2; a3],
                [],
                Operation(
                    MathOperation(
                        Fmax,
                        [Operation(
                            MathOperation(
                                Fmax,
                                [Symbol(a1);
                                 Symbol(a2)]
                            )
                         );
                         Symbol(a3)]
                    )
                )
            )
        ParseAndTestWithBindings xl_expr fp_expected bindings

    [<TestMethod>]
    member self.MAXExpr2() =
        // should be: (FPCore () (fmax (fmax (fmax 1 2) 3) 4))
        let xl_expr = "=MAX(1,2,3,4)"
        let fp_expected =
            FPCore(
                [],
                [],
                Operation(
                    MathOperation(
                        Fmax,
                        [Operation(
                            MathOperation(
                                Fmax,
                                [Operation(
                                    MathOperation(
                                        Fmax,
                                        [Num(1.0);
                                         Num(2.0)]
                                    )
                                 );
                                 Num(3.0)]
                            )
                         );
                         Num(4.0)]
                    )
                )
            )
        ParseAndTest xl_expr fp_expected

    [<TestMethod>]
    member self.SUMExpr1() =
        // should be: (FPCore (a1 a2 a3) (+ (+ a1 a2) a3))
        let xl_expr = "=SUM(A1:A3)"
        let bindings = EZBindings ["a1"; "a2"; "a3"]
        let fp_expected =
            let a1 = FPSymbol("a1")
            let a2 = FPSymbol("a2")
            let a3 = FPSymbol("a3")
            FPCore(
                [a1; a2; a3],
                [],
                Operation(
                    MathOperation(
                        Plus,
                        [Operation(
                            MathOperation(
                                Plus,
                                [Symbol(a1);
                                 Symbol(a2)]
                            )
                         );
                         Symbol(a3)]
                    )
                )
            )
        ParseAndTestWithBindings xl_expr fp_expected bindings

    [<TestMethod>]
    member self.PrecedenceTest() =
        // should be:
        let xl_expr = "=3*2+1"
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
                                [Num(3.0);
                                 Num(2.0)]
                            )
                        );
                        Num(1.0)]
                    )
                )
            )
        ParseAndTest xl_expr fp_expected

    [<TestMethod>]
    member self.ParensExpr1() =
        // should be:
        let xl_expr = "=(F9+G9)*B20"
        let bindings = EZBindings ["f9"; "g9"; "b20"]
        let fp_expected =
            let f9  = FPSymbol("f9")
            let g9  = FPSymbol("g9")
            let b20 = FPSymbol("b20")
            FPCore(
                [f9; g9; b20],
                [],
                Operation(
                    MathOperation(
                        Multiply,
                        [Parens(
                            Operation(
                                MathOperation(
                                    Plus,
                                    [Symbol(f9);
                                     Symbol(g9)]
                                )
                            )
                        );
                        Symbol(b20)]
                    )
                )
            )
        ParseAndTestWithBindings xl_expr fp_expected bindings

    [<TestMethod>]
    member self.ROUNDUPExpr1() =
        let xl_expr = "=ROUNDUP(H33,-1)"
        let bindings = EZBindings ["h33"]
        // should be:
        let fp_expected =
            let h33 = FPSymbol("h33")
            let B = FPSymbol("base")
            FPCore(
                [h33],
                [],
                Let(
                    FPLet(
                        [(B,
                          Operation(
                            MathOperation(
                                Pow,
                                [Num(10.0);
                                 Operation(
                                    UnaryOperation(
                                        Negation,
                                        Num(1.0)
                                    )
                                 )]
                            )
                          )
                        )],
                        Operation(
                            MathOperation(
                                Divide,
                                [If(
                                    FPIf(
                                        Operation(
                                            LogicalOperation(LessThan, [Symbol(h33); Num(0.0)])
                                        ),
                                        Operation(
                                            MathOperation(
                                                Floor,
                                                [Operation(
                                                    MathOperation(
                                                        Multiply,
                                                        [Symbol(h33); Symbol(B)]
                                                    )
                                                )]
                                            )
                                        ),
                                        Operation(
                                            MathOperation(
                                                Ceil,
                                                [Operation(
                                                    MathOperation(
                                                        Multiply,
                                                        [Symbol(h33); Symbol(B)]
                                                    )
                                                )]
                                            )
                                        )
                                    )
                                 );
                                 Symbol(B)]
                            )
                        )
                    )
                )
            )
        ParseAndTestWithBindings xl_expr fp_expected bindings

    [<TestMethod>]
    member self.IFExpr1() =
        let xl_expr = "=IF(TRUE,1,0)"
        let fp_expected =
            FPCore(
                [],
                [],
                If(
                    FPIf(
                        Bool(true),
                        Num(1.0),
                        Num(0.0)
                    )
                )
            )
        ParseAndTest xl_expr fp_expected

    [<TestMethod>]
    member self.IFExpr2() =
        let xl_expr = "=IF(A1,A2,A3)"
        let bindings = EZBindings(["a1"; "a2"; "a3"])
        let fp_expected =
            FPCore(
                [FPSymbol("a1");
                 FPSymbol("a2");
                 FPSymbol("a3")],
                [],
                If(
                    FPIf(
                        Symbol(FPSymbol("a1")),
                        Symbol(FPSymbol("a2")),
                        Symbol(FPSymbol("a3"))
                    )
                )
            )
        ParseAndTestWithBindings xl_expr fp_expected bindings