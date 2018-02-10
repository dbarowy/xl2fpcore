module XL2FPCore

open FPCoreAST

let rec XLExprToFPCore(expr: AST.Expression) : FPCore =
    match expr with
    | AST.ReferenceExpr(r) -> failwith "todo"
    | AST.BinOpExpr(op, e1, e2) -> failwith "todo"
    | AST.UnaryOpExpr(op, e) -> failwith "todo"
    | AST.ParensExpr(e) -> failwith "todo"