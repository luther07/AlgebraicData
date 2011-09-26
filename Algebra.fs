module Algebra
//Step 1 define a discriminated union for representing arithmetic expressions consisting of the following elements...
//Step 2 your type should support expressions of arbitrary depth and have a separate branch for each operator...
type Expression =
    | Integer           of int
    | Negation          of Expression
    | Addition          of Expression * Expression
    | Subtraction       of Expression * Expression
    | Multiplication    of Expression * Expression
    | Division          of Expression * Expression

//Define an evaluate function that evaluates such expressions like a calculator
let rec eval x =
    match x with
    | Integer(x)            -> x
    | Negation(x)           -> (-(eval(x)))
    | Addition(x, y)        -> (eval(x) + eval(y))
    | Subtraction(x, y)     -> (eval(x) - eval(y))
    | Multiplication(x, y)  -> (eval(x) * eval(y))
    | Division(x, y)        -> (eval(x) / eval(y))

//Define a size function that computes the number of nodes (operators and constants) in an expression.
let rec size x =
    match x with
    | Integer(_) -> 1
    | Negation(x) -> 1 + size(x)
    | Addition(x, y) -> 1 + size(x) + size(y)
    | Subtraction(x, y) -> 1 + size(x) + size(y)
    | Multiplication(x, y) -> 1 + size(x) + size(y)
    | Division(x, y) -> 1 + size(x) + size(y)