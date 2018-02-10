module XL2FPCore

open FPCoreAST
open System

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
    | :? AST.ReferenceRange as rng -> failwith "todo 4"
    | :? AST.ReferenceAddress as addr -> failwith "todo 5"
    | :? AST.ReferenceNamed as name -> failwith "todo 6"
    | :? AST.ReferenceFunction as func -> FunctionToFPExpr func
    | :? AST.ReferenceConstant as c -> Num(FPNum(c.Value))
    | :? AST.ReferenceString as str -> failwith "todo 8"
    | :? AST.ReferenceBoolean as b -> failwith "todo 9"

and FunctionToFPExpr(f: AST.ReferenceFunction) : FPExpr =
    match f.FunctionName with
    | "SUM" ->
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