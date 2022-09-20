namespace Homeworks

open System


module Main =

    let SimpleFunction x y = x + y


    let SimplePow (n: float) a =
        if n = 0.0 && a = 0 then
            printfn "Undefined"
            0.0
        elif n = 0.0 && a <> 0 then
            0.0
        elif n = 1.0 then
            1.0
        elif a < 0 then
            let mutable (ans: float) = 1.0

            for i = 1 to -a do
                ans <- n * ans

            1.0 / (float ans)
        else
            let mutable (ans: float) = 1

            for i = 1 to a do
                ans <- n * ans

            ans


    [<EntryPoint>]
    let main (argv: string array) =
        printfn "Choose what you want to do:\n1. SimplePow"
        let var = Console.ReadLine() |> int

        match var with
        | 1 -> printfn $"Answer is: %A{SimplePow 3 5}"
        | _ -> printfn "Error"

        0
