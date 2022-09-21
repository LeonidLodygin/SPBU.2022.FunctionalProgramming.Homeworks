namespace Homeworks

open System


module Main =

    let SimplePow (n: float) a =
        if n = 0.0 && a = 0 then
            printfn "Undefined"
            0.0
        elif n = 0.0 then
            0.0
        elif n = 1.0 then
            1.0
        elif a < 0 then
            let mutable (ans: float) = 1.0

            for i = 1 to -a do
                ans <- n * ans

            1.0 / (float ans)
        else
            let mutable (ans: float) = 1.0

            for i = 1 to a do
                ans <- n * ans

            ans



    (*let FastPow (n : float) a =
        if n = 0.0 && a = 0 then
            printfn "Undefined"
            0.0
        elif n = 0.0 then
            0.0
        elif n = 1.0 then
            1.0
        elif a >= 0 then
            *)


    let MinMaxFromArray (x: float []) =
        let mutable min = x[0]
        let mutable max = x[0]

        for i = 0 to x.Length - 1 do
            if min > x[i] then min <- x[i] else ()
            if max < x[i] then max <- x[i] else ()

        max - min



    [<EntryPoint>]
    let main (argv: string array) =
        printfn "Choose what you want to do:\n1. SimplePow\n3. MinMaxFromArray"
        let var = Console.ReadLine() |> int

        match var with
        | 1 -> printfn $"Answer is: %A{SimplePow -3 -2}"
        | 3 ->
            printfn
                $"Answer is: %A{MinMaxFromArray [| 1
                                                   2
                                                   3
                                                   4
                                                   5
                                                   6
                                                   7
                                                   9.2
                                                   0 |]}"
        | _ -> printfn "Error"

        0
