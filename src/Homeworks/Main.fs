namespace Homeworks

open GraphBuild

module Main =

    [<EntryPoint>]
    let main (argv: string array) =
        let x = System.Convert.ChangeType("1,1", typeof<float>)
        printfn $"%A{x}"
        let matrix = MatrixReader "C:\Users\Леонид\Desktop\hello2.mtx"
        for i in 1 .. 100 do
            printfn("Hello, world!")
        0
