module xl2fpcore

    let rec XLExprToFPCore(expr: AST.Expression) : string =
        match expr with
        | AST.ReferenceExpr(r) -> addrsFromRef r
        | AST.BinOpExpr(op, e1, e2) -> XLExprToFPCore e1 @ XLExprToFPCore e2
        | AST.UnaryOpExpr(op, e) -> XLExprToFPCore e
        | AST.ParensExpr(e) -> XLExprToFPCore e

    and addrsFromRef(ref: AST.Reference) : AST.Address list =
            match ref with
            | :? AST.ReferenceRange -> []
            | :? AST.ReferenceAddress as r -> addrsFromAddrRef r
            | :? AST.ReferenceNamed -> []   // TODO: symbol table lookup
            | :? AST.ReferenceFunction as r -> addrsFromFunctionRef r
            | :? AST.ReferenceConstant -> []
            | :? AST.ReferenceString -> []
            | :? AST.ReferenceBoolean -> []
            | _ -> raise (AST.ParseException "Unknown reference type.")

    and addrsFromAddrRef(ref: AST.ReferenceAddress) : AST.Address list = [ref.Address]

    and addrsFromFunctionRef(ref: AST.ReferenceFunction) : AST.Address list =
        List.map (fun arg -> addrsFromExpr(arg)) ref.ArgumentList |> List.concat

    let rec constantsFromExpr(expr: AST.Expression) : AST.ReferenceConstant list =
        match expr with
        | AST.ReferenceExpr(r) -> constantsFromRef r
        | AST.BinOpExpr(op, e1, e2) -> constantsFromExpr e1 @ constantsFromExpr e2
        | AST.UnaryOpExpr(op, e) -> constantsFromExpr e
        | AST.ParensExpr(e) -> constantsFromExpr e

    and constantsFromRef(ref: AST.Reference) : AST.ReferenceConstant list =
        match ref with
        | :? AST.ReferenceRange -> []
        | :? AST.ReferenceAddress as r -> []
        | :? AST.ReferenceNamed -> []
        | :? AST.ReferenceFunction as r -> r.ArgumentList |> List.map constantsFromExpr |> List.concat
        | :? AST.ReferenceConstant as r -> [r]
        | :? AST.ReferenceString -> []
        | :? AST.ReferenceBoolean -> []
        | _ -> raise (AST.ParseException "Unknown reference type.")