namespace Homeworks


module Main =

    let SimpleFunction x y = x + y

    let SimplePow n a =
        if n = 0 then
            0
        else if a = 0 then
            1
        else
            let mutable ans = 1

            for i = 1 to a do
                ans <- n * ans

            ans




    [<EntryPoint>]
    let main (argv: string array) =
        //printfn $"Sum of your integers is: %A{SimpleFunction 10 20}"
        printfn $"%A{SimplePow 3 0}"
        0
