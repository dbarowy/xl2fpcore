module XL2FPCore

open FPCoreAST
open System

let NormalizeAddr(a: AST.Address) =
    // TODO:  use "local" addresses for now;
    // really, we should come up with a scheme
    // to get a fresh variable for every new reference
    Symbol(FPSymbol(a.A1Local().ToLower()))

let ExpandPseudoList(e: FPExpr) : FPExpr list option =
    match e with
    | PseudoList(xs) -> Some xs
    | _ -> None

let rec FormulaToFPCore(expr: AST.Expression) : FPCore =
    FPCore([], [], ExprToFPExpr(expr))

and ExprToFPExpr(expr: AST.Expression) : FPExpr =
    match expr with
    | AST.ReferenceExpr(r) -> RefToFPExpr r
    | AST.BinOpExpr(op, e1, e2) -> failwith "todo 1"
    | AST.UnaryOpExpr(op, e) -> failwith "todo 2"
    | AST.ParensExpr(e) -> failwith "todo 3"

and RefToFPExpr(r: AST.Reference) : FPExpr =
    match r with
    | :? AST.ReferenceRange as rng -> 
        let addrs =
            Array.map NormalizeAddr (rng.Range.Addresses())
            |> Array.toList
        PseudoList(addrs)
    | :? AST.ReferenceAddress as addr -> NormalizeAddr (addr.Address)
    | :? AST.ReferenceNamed as name -> failwith "todo 6"
    | :? AST.ReferenceFunction as func -> FunctionToFPExpr func
    | :? AST.ReferenceConstant as c -> Num(FPNum(c.Value))
    | :? AST.ReferenceString as str -> failwith "todo 8"
    | :? AST.ReferenceBoolean as b -> failwith "todo 9"

and FunctionToFPExpr(f: AST.ReferenceFunction) : FPExpr =
    match f.FunctionName with
    | "SUM" ->
        // Convert args
        let fpargs = List.map (fun arg -> ExprToFPExpr arg) f.ArgumentList

        // If any args are PseudoFP constructs, desugar them


        let rec proc(args: AST.Expression list) : FPExpr option =
            match args with
            | x1 :: x2 :: [] ->
                Some (FPExpr.Operation(FPOperation.MathOperation(FPMathOperation.Plus, [ExprToFPExpr x1; ExprToFPExpr x2])))
            | x :: rest ->
                let xe = ExprToFPExpr x
                match proc rest with
                | Some(reste) -> Some (FPExpr.Operation(FPOperation.MathOperation(FPMathOperation.Plus, [xe; reste])))
                | None -> Some xe
            | [] -> None

        match proc (f.ArgumentList) with
        | Some expr -> expr
        | None -> Num(FPNum(0.0))   // Literally, SUM of nothing
        
    | _ -> raise (Exception ("Unknown function '" + (f.FunctionName) + "'"))