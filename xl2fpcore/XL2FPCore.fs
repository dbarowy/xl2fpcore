module XL2FPCore

open FPCoreAST
open System
open System.Collections.Generic

exception InvalidExpressionException of string

let rec UnrollWithOp(exprs: FPExpr list)(op: FPMathOperation) : FPExpr =
    match exprs with
    | x1 :: x2 :: [] -> Operation(MathOperation(op, [x2; x1]))
    | x1 :: [] -> x1
    | [] -> failwith "Does this actually happen?"
    | x1 :: rest -> Operation(MathOperation(op, [UnrollWithOp rest op; x1]))

let NormalizeAddr(a: AST.Address) =
    // TODO:  use "local" addresses for now;
    // really, we should come up with a scheme
    // to get a fresh variable for every new reference
    a.A1Local().ToLower()

let expandApplications(pre: List<Dictionary<AST.Address,double>>) : FPProperty option =
    if pre.Count > 0 then
        let logand =
            pre |>
            Seq.map (fun dict ->
                let exprs = 
                    dict |>
                    Seq.map (fun kvp ->
                        let name = (NormalizeAddr kvp.Key).ToString()
                        let value = kvp.Value.ToString()
                        "(== " + name + " " + value + ")"
                    ) |>
                    Seq.toList
                "(and " + String.Join(" ", exprs) + ")"
            )
        let expr = "(or " + String.Join(" ", logand) + ")"
        Some(PropString(FPSymbol("pre"), expr))
    else
        None

let rec FormulaToFPCore(expr: AST.Expression)(pre: List<Dictionary<AST.Address,double>>) : FPCore =
    let expr,args = ExprToFPExpr(expr)
    match expandApplications pre with
    | Some prop -> FPCore(args, [prop], expr)
    | None      -> FPCore(args, [], expr)

// returns a tuple of two things:
// first: the converted FPCore expression
// second: a list of symbols representing Excel references that
//         must be abstracted as FPCore arguments
and ExprToFPExpr(expr: AST.Expression) : FPExpr*FPSymbol list =
    match expr with
    | AST.ReferenceExpr(r) -> RefToFPExpr r
    | AST.BinOpExpr(op, e1, e2) -> BinOpToFPExpr op (ExprToFPExpr e1) (ExprToFPExpr e2)
    | AST.UnaryOpExpr(op, e) ->
        match op with
        | '+' -> ExprToFPExpr e     // basically, ignore it
        | '-' ->
            let e2,args = ExprToFPExpr e
            Operation(UnaryOperation(Negation, e2)), args
        | opchar -> raise (InvalidExpressionException("Unknown unary operator '" + opchar.ToString() + "'"))
    | AST.ParensExpr(e) ->
        let e1,exs = ExprToFPExpr e
        Parens(e1),exs
and RefToFPExpr(r: AST.Reference) : FPExpr*FPSymbol list =
    match r with
    | :? AST.ReferenceRange as rng -> 
        let addrs =
            Array.map (fun a -> FPSymbol(NormalizeAddr a)) (rng.Range.Addresses())
            |> Array.toList
        let pseudolist = addrs |> List.map (fun s -> Symbol(s)) |> (fun xs -> PseudoList(xs))
        pseudolist,addrs
    | :? AST.ReferenceAddress as addr ->
        let na = FPSymbol(NormalizeAddr (addr.Address))
        Symbol(na), [na]
    | :? AST.ReferenceNamed as name -> failwith "todo 6"
    | :? AST.ReferenceFunction as func -> FunctionToFPExpr func
    | :? AST.ReferenceConstant as c -> Num(c.Value),[]
    | :? AST.ReferenceString as str -> raise (InvalidExpressionException "FPCore does not support strings.")
    | :? AST.ReferenceBoolean as b -> failwith "todo 9"
    | :? AST.ReferenceUnion as ru ->
        let result = ru.References |> List.map (fun r -> ExprToFPExpr r)
        let refs, args = List.unzip result
        let arg = List.concat args
        PseudoList(refs), arg
    | _ -> failwith "Unknown reference expression."

and FunctionToFPExpr(f: AST.ReferenceFunction) : FPExpr*FPSymbol list =
    let expr,args =
        match f.FunctionName with
        | "AVERAGE" ->
            let sum,args = XLUnrollWithOpAndDefault f.ArgumentList Plus (Sentinel,[])
            let n = XLCountUnroll f.ArgumentList
            Operation(MathOperation(Divide, [sum; Num(double n)])), args
        | "ROUNDUP" -> ROUNDUP f.ArgumentList
        | "MAX" -> XLUnrollWithOpAndDefault f.ArgumentList Fmax (Sentinel,[])
        | "MIN" -> XLUnrollWithOpAndDefault f.ArgumentList Fmin (Sentinel,[])
        | "SUM" -> XLUnrollWithOpAndDefault f.ArgumentList Plus (Sentinel,[])       
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
    | _ -> failwith "Unknown binary operator"

and XLCountUnroll(exprs: AST.Expression list) : int =
    match exprs with
    | x :: rest ->
        let xe,_ = ExprToFPExpr x
        let count =
            match xe with
            | PseudoList(xes) -> List.length xes
            | _ -> 1
        count + XLCountUnroll rest
    | [] -> 0

and XLUnrollWithOpAndDefault(exprs: AST.Expression list)(op: FPMathOperation)(def: FPExpr*FPSymbol list) : FPExpr*FPSymbol list =
    let rec proc(exprs: AST.Expression list)(op: FPMathOperation) =
        // we must match in pairs because we're unrolling with a binary op;
        // we also must expand pseudolists (ranges) into FPCore expression lists
        match exprs with
        | x1 :: x2 :: [] ->
            let x1expr,x1args = ExprToFPExpr x1
            let x2expr,x2args = ExprToFPExpr x2
                
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
            let xe,xeargs = ExprToFPExpr x

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

// (let ([base (pow 10 B)]) (/ (if (< A 0) (floor (* A base)) (ceil (* A base))) base)
and ROUNDUP(args: AST.Expression list) =
    match args with
    | xl_num::xl_num_digits::[] -> 
        let num,args = ExprToFPExpr xl_num
        let num_digits,args2 = ExprToFPExpr xl_num_digits
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
