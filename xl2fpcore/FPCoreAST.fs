module FPCoreAST

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

type FPNum =
    | Int of BigInteger
    | Frac of BigInteger * BigInteger

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

and FPSymbol = string

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

and FPOperation =
    | FPLogicalOperation
    | FPMathOperation

and FPProperty =
    | PropExpr of FPSymbol * FPExpr
    | PropString of FPSymbol * string
    | PropSymbols of FPSymbol * FPSymbol list

and FPIf = FPExpr * FPExpr * FPExpr

and FPLet = (FPSymbol * FPExpr) list * FPExpr

and FPWhile = (FPSymbol * FPExpr * FPExpr) list * FPExpr

and FPExpr =
    | FPNum
    | FPConstant
    | FPSymbol
    | FPOperation

and FPCore = FPSymbol list * FPProperty list * FPExpr