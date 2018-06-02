module FPCoreAST

open System
open System.Numerics

//  FPCORE GRAMMAR
//    
//  FPCore
//      ( FPCore (symbol*) property* expr ) 
//  expr
//      number
//      constant
//      symbol
//      ( operation expr+ )
//      ( if expr expr expr )
//      ( let ( [ symbol expr ]* ) expr )
//      ( while expr ( [ symbol expr expr ]* ) expr )
//  property
//      :symbol expr
//      :symbol string
//      :symbol ( symbol* ) 

// newline
//let NL = System.Environment.NewLine

// indent function
let Ind(n: int) = String.replicate n " "

type FPNum(num: double) =
    member self.ToExpr(ind: int) =
        (Ind ind) + num.ToString()
    member self.Num = num
    override self.Equals(o: obj) =
        match o with
        | :? FPNum as fpn -> num = fpn.Num
        | _ -> false
    override self.GetHashCode() = num.GetHashCode()

and FPConstant =
    | E
    | LOG2E
    | LOG10E
    | LN2
    | LN10
    | PI
    | PI_2
    | PI_4
    | N1_PI
    | N2_PI
    | N2_SQRTPI
    | SQRT2
    | SQRT1_2
    | INFINITY
    | NAN
    member self.ToExpr(ind: int) =
        (Ind ind) +
        match self with
        | E         -> "E"
        | LOG2E     -> "LOG2E"
        | LOG10E    -> "LOG10E"
        | LN2       -> "LN2"
        | LN10      -> "LN10"
        | PI        -> "PI"
        | PI_2      -> "PI_2"
        | PI_4      -> "PI_4"
        | N1_PI     -> "1_PI"
        | N2_PI     -> "2_PI"
        | N2_SQRTPI -> "2_SQRTPI"
        | SQRT2     -> "SQRT2"
        | SQRT1_2   -> "SQRT1_2"
        | INFINITY  -> "INFINITY"
        | NAN       -> "NAN"

and FPSymbol(s: string) =
    member self.ToExpr(ind: int) = (Ind ind) + s
    member self.String = s
    override self.Equals(o: obj) =
        match o with
        | :? FPSymbol as fps -> self.String = fps.String
        | _ -> false
    override self.GetHashCode() = s.GetHashCode()

and FPLogicalOperation =
    | LessThan
    | GreaterThan
    | LessThanOrEqual
    | GreaterThanOrEqual
    | Equal
    | NotEqual
    | LAnd
    | LOr
    | LNot
    | IsFinite
    | IsInf
    | IsNaN
    | IsNormal
    | SignBit
    member self.ToExpr(int: int) =
        (Ind int) +
        match self with
        | LessThan              -> "<"
        | GreaterThan           -> ">"
        | LessThanOrEqual       -> "<="
        | GreaterThanOrEqual    -> ">="
        | Equal                 -> "=="
        | NotEqual              -> "!="
        | LAnd                  -> "and"
        | LOr                   -> "or"
        | LNot                  -> "not"
        | IsFinite              -> "isfinite"
        | IsInf                 -> "isinf"
        | IsNaN                 -> "isnan"
        | IsNormal              -> "isnormal"
        | SignBit               -> "signbit"

and FPMathOperation =
    | Plus
    | Minus
    | Multiply
    | Divide
    | Fabs
    | Fma
    | Exp
    | Exp2
    | Expm1
    | Log
    | Log10
    | Log2
    | Log1p
    | Pow
    | Sqrt
    | Cbrt
    | Hypot
    | Sin
    | Cos
    | Tan
    | Asin
    | Acos
    | Atan
    | Atan2
    | Sinh
    | Cosh
    | Tanh
    | Asinh
    | Acosh
    | Atanh
    | Erf
    | Erfc
    | Tgamma
    | Lgamma
    | Ceil
    | Floor
    | Fmod
    | Remainder
    | Fmax
    | Fmin
    | Fdim
    | Copysign
    | Trunc
    | Round
    | Nearbyint
    member self.ToExpr(ind: int) =
        (Ind ind) +
        match self with
        | Plus      -> "+"
        | Minus     -> "-"
        | Multiply  -> "*"
        | Divide    -> "/"
        | Fabs      -> "fabs"
        | Fma       -> "fma"
        | Exp       -> "exp"
        | Exp2      -> "exp2"
        | Expm1     -> "expm1"
        | Log       -> "log"
        | Log10     -> "log10"
        | Log2      -> "log2"
        | Log1p     -> "log1p"
        | Pow       -> "pow"
        | Sqrt      -> "sqrt"
        | Cbrt      -> "cbrt"
        | Hypot     -> "hypot"
        | Sin       -> "sin"
        | Cos       -> "cos"
        | Tan       -> "tan"
        | Asin      -> "asin"
        | Acos      -> "acos"
        | Atan      -> "atan"
        | Atan2     -> "atan2"
        | Sinh      -> "sinh"
        | Cosh      -> "cosh"
        | Tanh      -> "tanh"
        | Asinh     -> "asinh"
        | Acosh     -> "acosh"
        | Atanh     -> "atanh"
        | Erf       -> "erf"
        | Erfc      -> "erfc"
        | Tgamma    -> "tgamma"
        | Lgamma    -> "lgamma"
        | Ceil      -> "ceil"
        | Floor     -> "floor"
        | Fmod      -> "fmod"
        | Remainder -> "remainder"
        | Fmax      -> "fmax"
        | Fmin      -> "fmin"
        | Fdim      -> "fdim"
        | Copysign  -> "copysign"
        | Trunc     -> "trunc"
        | Round     -> "round"
        | Nearbyint -> "nearbyint"

and FPOperation =
    | LogicalOperation of FPLogicalOperation * FPExpr list
    | MathOperation of FPMathOperation * FPExpr list
    member self.ToExpr(ind: int) =
        match self with
        | LogicalOperation(op, exprs) ->
            let exprStr = String.Join(" ", List.map (fun (e: FPExpr) -> e.ToExpr 0) exprs)
            (Ind ind) + "(" + op.ToExpr 0 + " " + exprStr + ")"
        | MathOperation(op, exprs) ->
            let exprStr = String.Join(" ", List.map (fun (e: FPExpr) -> e.ToExpr 0) exprs)
            (Ind ind) + "(" + op.ToExpr 0 + " " + exprStr + ")"

and [<CustomEquality; NoComparison>] FPProperty =
    | PropExpr of FPSymbol * FPExpr
    | PropString of FPSymbol * string
    | PropSymbols of FPSymbol * FPSymbol list
    member self.ToExpr(ind: int) =
        (Ind ind) +
        ":" +
        match self with
        | PropExpr(s,e) -> s.ToExpr 0 + Ind 1 + e.ToExpr 0
        | PropString(s,s') -> s.ToExpr 0 + s'
        | PropSymbols(s,xs) ->
            let symbs = String.Join(" ", List.map (fun (sym: FPSymbol) -> sym.ToExpr 0) xs)
            (s.ToExpr 0) + symbs
    override self.Equals(o: obj) =
        match o with
        | :? FPProperty as fpp ->
            match self,fpp with
            | PropExpr(s,e),PropExpr(s2,e2) ->
                s = s2 && e = e2
            | PropString(s,s'),PropString(s2,s2') ->
                s = s2 && s' = s2'
            | PropSymbols(s,xs),PropSymbols(s2,xs2) ->
                s = s2 && xs = xs2
            | _ -> false
        | _ -> false
    override self.GetHashCode(): int =
        match self with
        | PropExpr(s,e) -> s.GetHashCode() + e.GetHashCode()
        | PropString(s,s') -> s.GetHashCode() + s'.GetHashCode()
        | PropSymbols(s,xs) -> s.GetHashCode() + xs.GetHashCode()

and FPIf(cond: FPExpr, dotrue: FPExpr, dofalse: FPExpr) =
    // ( if expr expr expr )
    member self.ToExpr(ind: int) =
        (Ind ind) + "(if " + cond.ToExpr 0 + (Ind 1) +
        dotrue.ToExpr (ind + 1) +
        dofalse.ToExpr (ind + 1) + ")"

and FPLet(binds: (FPSymbol * FPExpr) list, in_expr: FPExpr) =
    // ( let ( [ symbol expr ]* ) expr )
    member self.ToExpr(ind: int) =
        let bindsStr = String.Join(
                        " ",
                        List.map (fun (s: FPSymbol, e: FPExpr) ->
                          "[" + (s.ToExpr 0) + " " + (e.ToExpr 0) + "]"
                        ) binds)
        (Ind ind) + "(let (" + bindsStr + ")" + (Ind 1) + in_expr.ToExpr (ind + 1) + ")"

and FPWhile(cond: FPExpr, binds: (FPSymbol*FPExpr*FPExpr) list, body: FPExpr) =
    // ( while expr ( [ symbol expr expr ]* ) expr )
    member self.ToExpr(ind: int) =
        let bindsStr = String.Join(
                         " ",
                         List.map (fun (s: FPSymbol, e1: FPExpr, e2: FPExpr) ->
                            "[" + s.ToExpr 0 + " " + e1.ToExpr 0 + e2.ToExpr 0 + "]"
                         )
                       )
        (Ind ind) + "(while" + cond.ToExpr 0 + "(" + bindsStr + ")" + body.ToExpr 0 + ")"

and [<CustomEquality; NoComparison>] FPExpr =
    | Num of FPNum
    | Constant of FPConstant
    | Symbol of FPSymbol
    | Operation of FPOperation
    | If of FPIf
    | Let of FPLet
    | While of FPWhile
    | PseudoList of FPExpr list    // added by me; not present in FPCore
    | Sentinel                     // added by me; signals a nonsensical construction
    member self.ToExpr(ind: int) : string =
        match self with
        | Num(n) -> n.ToExpr ind
        | Constant(c) -> c.ToExpr ind
        | Symbol(s) -> s.ToExpr ind
        | Operation(op) -> op.ToExpr ind
        | If(e) -> e.ToExpr ind
        | Let(e) -> e.ToExpr ind
        | While(e) -> e.ToExpr ind
        | PseudoList(xs) -> "[" + String.Join(",", (List.map (fun (s: FPExpr) -> s.ToExpr ind) xs)) + "]"
        | Sentinel -> failwith "Cannot convert sentinel value to expression."
    override self.Equals(o: obj) =
        match o with
        | :? FPExpr as fpe ->
            match self,fpe with
            | Num(n),Num(n2) -> n = n2
            | Constant(c),Constant(c2) -> c = c2
            | Symbol(s),Symbol(s2) -> s = s2
            | Operation(o),Operation(o2) -> o = o2
            | If(e),If(e2) -> e = e2
            | Let(e),Let(e2) -> e = e2
            | While(e),While(e2) -> e = e2
            | PseudoList(xs),PseudoList(xs2) -> xs = xs2
            | _ -> false
        | _ -> false
    override self.GetHashCode() =
        match self with
        | Num(n) -> n.GetHashCode()
        | Constant(c) -> c.GetHashCode()
        | Symbol(s) -> s.GetHashCode()
        | Operation(op) -> op.GetHashCode()
        | If(e) -> e.GetHashCode()
        | Let(e) -> e.GetHashCode()
        | While(e) -> e.GetHashCode()
        | PseudoList(xs) -> xs.GetHashCode()
        | Sentinel -> failwith "Cannot get hashcode for sentinel value."
    //override self.ToString() = self.ToExpr(0)

and FPCore(args: FPSymbol list, props: FPProperty list, body: FPExpr) =
    // (FPCore (x)
    //  :name "NMSE example 3.1"
    //  :cite (hamming-1987)
    //  :pre (>= x 0)
    //  (- (sqrt (+ x 1)) (sqrt x)))
    member self.ToExpr(ind: int) =
        let argStr = String.Join(" ", List.map (fun (arg: FPSymbol) -> arg.ToExpr 0) args)
        let popStr = String.Join(" ", List.map (fun (prop: FPProperty) -> prop.ToExpr (ind + 1)) props)
        (Ind ind) + "(FPCore (" + argStr + ")" + " " + popStr + " " + body.ToExpr (ind + 1) + ")"
    member self.Arguments = args
    member self.Properties = props
    member self.Body = body
    override self.Equals(o: obj) =
        match o with
        | :? FPCore as fpc ->
            self.Arguments = fpc.Arguments &&
            self.Properties = fpc.Properties &&
            self.Body = fpc.Body
        | _ -> false
    override self.GetHashCode() = (args,props,body).GetHashCode()