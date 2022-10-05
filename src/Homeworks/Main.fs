namespace Homeworks


module Main =

    let SimpleFunction x y = x + y

    [<EntryPoint>]
    let main (argv: string array) =
        printfn $"Sum of your integers is: %A{SimpleFunction 10 20}"
        0
