module mainOLD

open Algebra

printfn "%s" "Hello World"
printfn "%s" ((eval(Integer(1))).ToString())
printfn "%s" ((eval(Addition(Integer(1), Integer(1)))).ToString())
printfn "%s" ((eval(Subtraction(Integer(1), Integer(1)))).ToString())
printfn "%s" ((eval(Multiplication(Integer(1), Integer(1)))).ToString())
printfn "%s" ((eval(Division(Integer(1), Integer(1)))).ToString())

