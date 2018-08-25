module XL2FPCore

open FPCoreAST
open System
open System.Collections.Generic
open System.Runtime.Remoting

type Bindings =  Dictionary<AST.Address*bool*bool,string>
type Provenance = AST.Address[]

type InvalidExpressionException(message: string) =
    inherit Exception(message)

let rec UnrollWithOp(exprs: FPExpr list)(op: FPMathOperation) : FPExpr =
    match exprs with
    | x1 :: x2 :: [] -> Operation(MathOperation(op, [x2; x1]))
    | x1 :: [] -> x1
    | [] -> failwith "Does this actually happen?"
    | x1 :: rest -> Operation(MathOperation(op, [UnrollWithOp rest op; x1]))

let AddrToKey(a: AST.Address) : AST.Address*bool*bool =
    a, a.ColMode = AST.AddressMode.Absolute, a.RowMode = AST.AddressMode.Absolute

let NormalizeAddr(a: AST.Address)(bindings: Bindings) =
    bindings.[AddrToKey a]

let expandApplications(pre: List<Dictionary<string,double>>) : FPProperty list =
    if pre.Count > 0 then
        let logand =
            pre |>
            Seq.map (fun dict ->
                let exprs = 
                    dict |>
                    Seq.map (fun kvp ->
                        let name = kvp.Key
                        let value = kvp.Value.ToString()
                        "(== " + name + " " + value + ")"
                    ) |>
                    Seq.toList
                "(and " + String.Join(" ", exprs) + ")"
            )
        let expr = "(or " + String.Join(" ", logand) + ")"
        [PropString(FPSymbol("pre"), expr)]
    else
        []

let expandProvenance(prov: Provenance) : FPProperty list =
    if prov.Length > 0 then
        let prov_props =
            prov |>
            Array.map (fun addr ->
                // By default, addresses are absolute; make them relative for props
                let a1_rel = AST.Address.fromR1C1withMode(addr.Row, addr.Col, AST.AddressMode.Relative, AST.AddressMode.Relative, addr.WorksheetName, addr.WorkbookName, addr.Path)
                "{ :excel_workbook \""    + addr.WorkbookName     + "\"," +
                 " :excel_worksheet \""   + addr.WorksheetName    + "\"," +
                 " :excel_cell \""        + a1_rel.A1Local()      + "\" }"
            )
        let expr = "[ " + String.Join(", ", prov_props) + " ]"
        [PropString(FPSymbol("excel_source"), expr)]
    else
        []

let rec FormulaToFPCore(expr: AST.Expression)(pre: List<Dictionary<string,double>>)(bindings: Bindings)(prov: Provenance) : FPCore =
    let expr',args = ExprToFPExpr expr bindings
    let props = expandApplications pre @ expandProvenance prov
    FPCore(args, props, expr')

and FormulaToFPCoreSimple(expr: AST.Expression) : FPCore =
    let pre = new List<Dictionary<string,double>>()
    let bindings = new Bindings()
    let provenance = [||]
    FormulaToFPCore expr pre bindings provenance

// returns a tuple of two things:
// first: the converted FPCore expression
// second: a list of symbols representing Excel references that
//         must be abstracted as FPCore arguments
and ExprToFPExpr(expr: AST.Expression)(bindings: Bindings) : FPExpr*FPSymbol list =
    let expr',args =
        match expr with
        | AST.ReferenceExpr(r) -> RefToFPExpr r bindings
        | AST.BinOpExpr(op, e1, e2) -> BinOpToFPExpr op (ExprToFPExpr e1 bindings) (ExprToFPExpr e2 bindings)
        | AST.UnaryOpExpr(op, e) ->
            match op with
            | '+' -> ExprToFPExpr e bindings    // basically, ignore it
            | '-' ->
                let e2,args = ExprToFPExpr e bindings
                Operation(UnaryOperation(Negation, e2)), args
            | opchar -> raise (InvalidExpressionException("Unknown unary operator '" + opchar.ToString() + "'" + " in expression " + e.ToFormula))
        | AST.ParensExpr(e) ->
            let e1,exs = ExprToFPExpr e bindings
            Parens(e1),exs
    // make args distinct & sort by variable name
    let args' =
        args
        |> List.distinct
        |> List.sortBy (fun arg -> Convert.ToUInt32 (arg.String.Substring(1, arg.String.Length - 1)))
    expr', (args')
and RefToFPExpr(r: AST.Reference)(bindings: Bindings) : FPExpr*FPSymbol list =
    match r with
    | :? AST.ReferenceRange as rng -> 
        let addrs = rng.Range.Addresses()
        let variables =
            Array.map (fun a -> FPSymbol(NormalizeAddr a bindings)) addrs
            |> Array.toList
        let pseudolist = variables |> List.map (fun s -> Symbol(s)) |> (fun xs -> PseudoList(xs))
        pseudolist,variables
    | :? AST.ReferenceAddress as addr ->
        let na = FPSymbol(NormalizeAddr addr.Address bindings)
        Symbol(na), [na]
    | :? AST.ReferenceNamed as name -> failwith "todo 6"
    | :? AST.ReferenceFunction as func -> FunctionToFPExpr func bindings
    | :? AST.ReferenceConstant as c -> Num(c.Value),[]
    | :? AST.ReferenceString as str ->
        // is it the empty string? if so, the user probably wants 0
        if String.IsNullOrEmpty str.Value then
            Num(0.0),[]
        else
            raise (InvalidExpressionException ("FPCore does not support strings in expression '" + str.ToFormula + "' in workbook '" + str.WorkbookName + "' on worksheet '" + str.WorksheetName + "'"))
    | :? AST.ReferenceBoolean as b -> Bool(b.Value),[]
    | :? AST.ReferenceUnion as ru ->
        let result = ru.References |> List.map (fun r -> ExprToFPExpr r bindings)
        let refs, args = List.unzip result
        let arg = List.concat args
        PseudoList(refs), arg
    | _ -> failwith "Unknown reference expression."

and FunctionToFPExpr(f: AST.ReferenceFunction)(bindings: Bindings) : FPExpr*FPSymbol list =
    let expr,args =
        match f.FunctionName with
        | "AND"     -> AND f.ArgumentList bindings
        | "AVERAGE" -> AVERAGE f.ArgumentList bindings
        | "IF"      -> IF f.ArgumentList bindings
        | "MAX"     -> XLUnrollWithOpAndDefault f.ArgumentList Fmax (Sentinel,[]) bindings
        | "MIN"     -> XLUnrollWithOpAndDefault f.ArgumentList Fmin (Sentinel,[]) bindings
        | "NOT"     -> NOT f.ArgumentList bindings
        | "OR"      -> OR f.ArgumentList bindings
        | "ROUNDUP" -> ROUNDUP f.ArgumentList bindings
        | "STDEVP"  -> STDEVP f.ArgumentList bindings
        | "STDEV.P" -> STDEVP f.ArgumentList bindings
        | "SUM"     -> XLUnrollWithOpAndDefault f.ArgumentList Plus (Sentinel,[]) bindings   
        | "XOR"     -> XOR f.ArgumentList bindings
        | _ -> raise (Exception ("Unknown function '" + (f.FunctionName) + "'"))
    
    match expr with
    | Sentinel -> raise (Exception ("Expression is not valid."))
    | _ -> expr,args

and BinOpToFPExpr(op: string)(e1: FPExpr*FPSymbol list)(e2: FPExpr*FPSymbol list) : FPExpr*FPSymbol list =
    match op with
    | "+" -> Operation(MathOperation(Plus, [fst e1; fst e2])), (snd e1) @ (snd e2)
    | "-" -> Operation(MathOperation(Minus, [fst e1; fst e2])), (snd e1) @ (snd e2)
    | "*" -> Operation(MathOperation(Multiply, [fst e1; fst e2])), (snd e1) @ (snd e2)
    | "/" -> Operation(MathOperation(Divide, [fst e1; fst e2])), (snd e1) @ (snd e2)
    | "^" -> Operation(MathOperation(Pow, [fst e1; fst e2])), (snd e1) @ (snd e2)
    | ">" -> Operation(LogicalOperation(GreaterThan, [fst e1; fst e2])), (snd e1) @ (snd e2)
    | "<" -> Operation(LogicalOperation(LessThan, [fst e1; fst e2])), (snd e1) @ (snd e2)
    | ">=" -> Operation(LogicalOperation(GreaterThanOrEqual, [fst e1; fst e2])), (snd e1) @ (snd e2)
    | "<=" -> Operation(LogicalOperation(LessThanOrEqual, [fst e1; fst e2])), (snd e1) @ (snd e2)
    | "<>" -> Operation(LogicalOperation(NotEqual, [fst e1; fst e2])), (snd e1) @ (snd e2)
    | "=" -> Operation(LogicalOperation(Equal, [fst e1; fst e2])), (snd e1) @ (snd e2)
    | _ -> failwith ("Unknown binary operator in expression \"" + (fst e1).ToString() + " " + op + " " + (fst e2).ToString() + "\"")

and XLCountUnroll(exprs: AST.Expression list)(bindings: Bindings) : int =
    match exprs with
    | x :: rest ->
        let xe,_ = ExprToFPExpr x bindings
        let count =
            match xe with
            | PseudoList(xes) -> List.length xes
            | _ -> 1
        count + XLCountUnroll rest bindings
    | [] -> 0

and XLMap(exprs: AST.Expression list)(f: FPExpr -> FPExpr)(bindings: Bindings) : FPExpr list*FPSymbol list =
    match exprs with
    | x :: rest ->
        let xe,a = ExprToFPExpr x bindings
        let fps =
            match xe with
            | PseudoList(xes) ->
                xes |> List.map (fun fpexpr -> f fpexpr)
            | _ -> [ f xe ]
        let fps_r, a_r = XLMap rest f bindings
        fps @ fps_r, a @ a_r
    | [] -> [], []

and XLUnrollWithOpAndDefault(exprs: AST.Expression list)(op: FPMathOperation)(def: FPExpr*FPSymbol list)(bindings: Bindings) : FPExpr*FPSymbol list =
    let rec proc(exprs: AST.Expression list)(op: FPMathOperation) =
        // we must match in pairs because we're unrolling with a binary op;
        // we also must expand pseudolists (ranges) into FPCore expression lists
        match exprs with
        | x1 :: x2 :: [] ->
            let x1expr,x1args = ExprToFPExpr x1 bindings
            let x2expr,x2args = ExprToFPExpr x2 bindings
                
            let x1unroll =
                match x1expr with
                | PseudoList(x1s) -> UnrollWithOp (List.rev x1s) op
                | _ -> x1expr

            let x2unroll = 
                match x2expr with
                | PseudoList(x2s) ->  UnrollWithOp (List.rev x2s) op
                | _ -> x2expr

            Some (Operation(MathOperation(op, [x2unroll; x1unroll])), x1args @ x2args)
        | x :: rest ->
            let xe,xeargs = ExprToFPExpr x bindings

            let xeunroll =
                match xe with
                | PseudoList(xes) -> UnrollWithOp (List.rev xes) op
                | _ -> xe

            match proc rest op with
            | Some(reste, restargs) -> Some (Operation(MathOperation(op, [reste; xeunroll])), xeargs @ restargs)
            | None -> Some (xeunroll,xeargs)
        | [] -> None
    match proc (List.rev exprs) op with
    | Some expr -> expr
    | None -> def

and AVERAGE(args: AST.Expression list)(bindings: Bindings) =
    let sum,args' = XLUnrollWithOpAndDefault args Plus (Sentinel,[]) bindings
    let n = XLCountUnroll args bindings
    Operation(MathOperation(Divide, [sum; Num(double n)])), args'

and STDEVP(args: AST.Expression list)(bindings: Bindings) =
    let aexpr,margs = AVERAGE args bindings
    let aexpr_symb = FPSymbol("mean")
    let dexprs,dargs = XLMap args (fun fexpr -> Operation(MathOperation(Pow, [Operation(MathOperation(Minus, [fexpr; Symbol(aexpr_symb)])); Num(2.0)]))) bindings
    let mean_diff = Operation(MathOperation(Divide, [UnrollWithOp dexprs Plus; Num(double dexprs.Length)]))
    let sqrt = Operation(MathOperation(Sqrt, [mean_diff]))
    Let(
        FPLet(
            [(aexpr_symb, aexpr)],
            sqrt
        )
    ), margs @ dargs

and AND(args: AST.Expression list)(bindings: Bindings) =
    let exprs,argss = args |> List.map (fun arg -> ExprToFPExpr arg bindings) |> List.unzip
    let args' = List.concat argss
    Operation(LogicalOperation(LAnd, exprs)), args'

and OR(args: AST.Expression list)(bindings: Bindings) =
    let exprs,argss = args |> List.map (fun arg -> ExprToFPExpr arg bindings) |> List.unzip
    let args' = List.concat argss
    Operation(LogicalOperation(LOr, exprs)), args'

and NOT(args: AST.Expression list)(bindings: Bindings) =
    let exprs,argss = args |> List.map (fun arg -> ExprToFPExpr arg bindings) |> List.unzip
    let args' = List.concat argss
    Operation(LogicalOperation(LNot, exprs)), args'

and nand(arg1: FPExpr)(arg2: FPExpr) =
    Operation(LogicalOperation(LNot, [Operation(LogicalOperation(LAnd, [arg1; arg2]))]))

and xor(arg1: FPExpr)(arg2: FPExpr) =
    nand (nand arg1 (nand arg1 arg2)) (nand arg2 (nand arg1 arg2))

and XOR(args: AST.Expression list)(bindings: Bindings) =
    let fpexprs,argss = args |> List.map (fun arg -> ExprToFPExpr arg bindings) |> List.unzip
    let args' = argss |> List.concat
    if args.Length > 1 then
        let f = (fun a b -> xor a b)
        List.reduce f fpexprs, args'
    else
        List.head fpexprs, args'

and IF(args: AST.Expression list)(bindings: Bindings) =
    match args with
    | cond :: if_expr :: else_expr :: nil -> 
        let c_fp, c_args = ExprToFPExpr cond bindings
        let i_fp, i_args = ExprToFPExpr if_expr bindings
        let e_fp, e_args = ExprToFPExpr else_expr bindings
        If(FPIf(
            c_fp,
            i_fp,
            e_fp
        )), c_args @ i_args @ e_args
    | _ ->
        raise (Exception(("IF expression must have exactly three arguments; expression has " + args.Length.ToString())))

// (let ([base (pow 10 B)]) (/ (if (< A 0) (floor (* A base)) (ceil (* A base))) base)
and ROUNDUP(args: AST.Expression list)(bindings: Bindings) =
    match args with
    | xl_num::xl_num_digits::[] -> 
        let num,args = ExprToFPExpr xl_num bindings
        let num_digits,args2 = ExprToFPExpr xl_num_digits bindings
        // factor = 10^num_digits
        // we multiply num by factor, take the ceiling, then divide by factor
        let B = FPSymbol("base")
        let factor =
            Operation(
                MathOperation(
                    Pow,
                    [Num(10.0); num_digits]
                )
            )
        Let(
            FPLet(
                [(B, factor)],
                Operation(
                    MathOperation(
                        Divide,
                        [If(
                            FPIf(
                                Operation(
                                    LogicalOperation(LessThan, [num; Num(0.0)])
                                ),
                                Operation(
                                    MathOperation(
                                        Floor,
                                        [Operation(
                                            MathOperation(
                                                Multiply,
                                                [num; Symbol(B)]
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
                                                [num; Symbol(B)]
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
        ), args @ args2
    | _ -> raise (InvalidExpressionException("ROUNDUP: Unrecognized number of arguments."))
