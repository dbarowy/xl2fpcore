﻿module XL2FPCore

open FPCoreAST
open System

let rec UnrollWithOp(exprs: FPExpr list)(op: FPMathOperation) : FPExpr =
    match exprs with
    | x1 :: x2 :: [] ->
        FPExpr.Operation(FPOperation.MathOperation(op, [x2; x1]))
    | x1 :: [] -> x1
    | [] -> failwith "Does this actually happen?"
    | x1 :: rest ->
        FPExpr.Operation(FPOperation.MathOperation(op, [UnrollWithOp rest op; x1]))

let NormalizeAddr(a: AST.Address) =
    // TODO:  use "local" addresses for now;
    // really, we should come up with a scheme
    // to get a fresh variable for every new reference
    FPSymbol(a.A1Local().ToLower())

let ExpandPseudoList(e: FPExpr) : FPExpr list option =
    match e with
    | PseudoList(xs) -> Some xs
    | _ -> None

let rec FormulaToFPCore(expr: AST.Expression) : FPCore =
    let expr,args = ExprToFPExpr(expr)
    FPCore(args, [], expr)

and ExprToFPExpr(expr: AST.Expression) : FPExpr*FPSymbol list =
    match expr with
    | AST.ReferenceExpr(r) -> RefToFPExpr r
    | AST.BinOpExpr(op, e1, e2) -> failwith "todo 1"
    | AST.UnaryOpExpr(op, e) -> failwith "todo 2"
    | AST.ParensExpr(e) -> failwith "todo 3"

and RefToFPExpr(r: AST.Reference) : FPExpr*FPSymbol list =
    match r with
    | :? AST.ReferenceRange as rng -> 
        let addrs =
            Array.map (fun a -> NormalizeAddr a) (rng.Range.Addresses())
            |> Array.toList
        let pseudolist = addrs |> List.map (fun s -> Symbol(s)) |> (fun xs -> PseudoList(xs))
        pseudolist,addrs
    | :? AST.ReferenceAddress as addr ->
        let na = NormalizeAddr (addr.Address)
        Symbol(na), [na]
    | :? AST.ReferenceNamed as name -> failwith "todo 6"
    | :? AST.ReferenceFunction as func -> FunctionToFPExpr func
    | :? AST.ReferenceConstant as c -> Num(FPNum(c.Value)),[]
    | :? AST.ReferenceString as str -> failwith "todo 8"
    | :? AST.ReferenceBoolean as b -> failwith "todo 9"
    | _ -> failwith "Unknown reference expression."

and FunctionToFPExpr(f: AST.ReferenceFunction) : FPExpr*FPSymbol list =
    match f.FunctionName with
    | "SUM" -> XLUnrollWithOpAndDefault (List.rev f.ArgumentList) Plus (Num(FPNum(0.0)),[])       
    | _ -> raise (Exception ("Unknown function '" + (f.FunctionName) + "'"))

and XLUnrollWithOpAndDefault(exprs: AST.Expression list)(op: FPMathOperation)(def: FPExpr*FPSymbol list) : FPExpr*FPSymbol list =
    let rec proc(exprs: AST.Expression list)(op: FPMathOperation) =
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

            Some (FPExpr.Operation(FPOperation.MathOperation(op, [x1unroll; x2unroll])), x1args @ x2args)
        | x :: rest ->
            let xe,xeargs = ExprToFPExpr x

            let xeunroll =
                match xe with
                | PseudoList(xes) -> UnrollWithOp (List.rev xes) op
                | _ -> xe

            match proc rest op with
            | Some(reste, restargs) -> Some (FPExpr.Operation(FPOperation.MathOperation(op, [xeunroll; reste])), xeargs @ restargs)
            | None -> Some (xeunroll,xeargs)
        | [] -> None
    match proc exprs op with
    | Some expr -> expr
    | None -> def