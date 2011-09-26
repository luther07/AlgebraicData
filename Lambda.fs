module Lambda
//You will define an algebraic data type (discriminated union type) that includes variants for arithmetic expressions, along with
//the following functional language types:

//#1    lambda x. e:        anonymous function, that is, a designated formal argument x along with a function body in which x occurs zero or more times
//#2    x:                  variable (presumably occurring in an expression with ancestor that is a lambda expression introducing x as a formal argument)
//#3    e1 e2:              function application, that is, an expression (presumably representing a function) followed by another expression (presumably representing an actual argument)
//#4    if c then e1 else   e2: conditional
//#5    Rec:                fixed-point operator, which can be applied to an expression e to provide a declaration-free equivalent of recursion
//      Rec is incomplete   Rec does not work

type Expression =
    | Integer           of int
    | Negation          of Expression
    | Addition          of Expression * Expression
    | Subtraction       of Expression * Expression
    | Multiplication    of Expression * Expression
    | Division          of Expression * Expression
    | Var               of string
    | App               of Expression * Expression
    | If                of Expression * Expression * Expression
    | Rec               of Expression
    | Fun               of string * Expression * Map<string, Expression>
    | Cell              of Expression * Expression
    | Car               of Expression
    | Cdr               of Expression

let lambda(x:string, b: Expression) = Fun(x, b, Map.empty)

let rec eval (x, env: Map<string,Expression>) =
    let myvalue y = 
        match eval(y, env) with
            | Integer(i)            -> i
            | Negation(x)           -> failwith "this match case is not possible"
            | Addition(x,y)         -> failwith "this match case is not possible"
            | Subtraction(x,y)      -> failwith "this match case is not possible"
            | Multiplication(x,y)   -> failwith "this match case is not possible"
            | Division(x,y)         -> failwith "this match case is not possible"
            | Var(x)                -> failwith "this match case is not possible"
            | App(x,y)              -> failwith "this match case is not possible"
            | If(x,y,z)             -> failwith "this match case is not possible"
            | Fun(x,y,z)            -> failwith "this match case is not possible"
            | Rec(x)                -> failwith "this match case is not possible"
            | Cell(x, y)            -> failwith "this match case is not possible"
            | Car(x)                -> failwith "this match case is not possible"
            | Cdr(x)                -> failwith "this match case is not possible"
    match x with
        | Integer(x)                -> Integer(x)
        | Negation(x)               -> Subtraction(Integer 0, x)
        | Addition(x,y)             -> Integer((myvalue(x) + (myvalue(y))))                              
        | Subtraction(x,y)          -> Integer((myvalue(x) - (myvalue(y))))                                  
        | Multiplication(x,y)       -> Integer((myvalue(x) * (myvalue(y))))                                  
        | Division(x,y)             -> Integer((myvalue(x) / (myvalue(y))))                                  
        | Var(x)                    -> match x with
                                        | string    -> env.Item(x)
        | App(e1, e2)               -> match eval(e1, env) with
                                        | Fun(str, b, env2)        -> eval(b, env.Add(str, eval(e2, env)))
                                        | Integer(x)                -> failwith "arg e1 to App must evaluate to type Fun"
                                        | Negation(x)               -> failwith "arg e1 to App must evaluate to type Fun"
                                        | Addition(x, y)            -> failwith "arg e1 to App must evaluate to type Fun"
                                        | Subtraction(x, y)         -> failwith "arg e1 to App must evaluate to type Fun"
                                        | Multiplication(x, y)      -> failwith "arg e1 to App must evaluate to type Fun"
                                        | Division(x, y)            -> failwith "arg e1 to App must evaluate to type Fun"
                                        | Var(x)                    -> failwith "arg e1 to App must evaluate to type Fun"
                                        | App(x, y)                 -> failwith "arg e1 to App must evaluate to type Fun"
                                        | If(x, y, z)               -> failwith "arg e1 to App must evaluate to type Fun"
                                        | Rec(x)                    -> failwith "arg e1 to App must evaluate to type Fun"
                                        | Cell(x, y)                -> failwith "arg e1 to App must evaluate to type Fun"
                                        | Car(x)                    -> failwith "arg e1 to App must evaluate to type Fun"
                                        | Cdr(x)                    -> failwith "arg e1 to App must evaluate to type Fun"
        | If(x, y, z)               -> if eval(x, env) = Integer(0) then
                                        z
                                        else y
        | Fun(x, b, env)           -> Fun(x, b, env)
        | Rec(x)                    -> match eval(x, env) with
                                        | Integer(x)                -> Integer(x)
                                        | Fun(x, b, env2)           -> eval(b, env2.Add(x,b))
                                        | Negation(x)               -> failwith "case is not valid"
                                        | Addition(x, y)            -> failwith "case is not valid"
                                        | Subtraction(x, y)         -> failwith "case is not valid"
                                        | Multiplication(x, y)      -> failwith "case is not valid"
                                        | Division(x, y)            -> failwith "case is not valid"
                                        | Var(x)                    -> failwith "case is not valid"
                                        | App(x, y)                 -> failwith "case is not valid"
                                        | If(x, y, z)               -> failwith "case is not valid"
                                        | Rec(x)                    -> failwith "case is not valid"
                                        | Cell(x, y)                -> failwith "case is not valid"
                                        | Car(x)                    -> failwith "case is not valid"
                                        | Cdr(x)                    -> failwith "case is not valid"
        | Cell(x, y)                -> Cell(eval(x, env), eval(y, env))
        | Car(x)                    -> match x with
                                        | Cell(x, y)    ->x
                                        | (_)           ->failwith "Car(x) should always evaluate to Cell(x,y)"
        | Cdr(x)                    -> match x with
                                        | Cell(x, y)    ->y
                                        | (_)           ->failwith "Cdr(x) should always evaluate to Cell(x,y)"
