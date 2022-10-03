namespace Homeworks
//open System

module Main =

    // Simple exponentiation of a number.
    let SimplePow (n: float) a =
        // Checking the base and exponent. We don't want to multiply 0 or 1 by itself.
        if n = 0.0 && a = 0 then
            failwith "0 to the power of 0 is uncertainty"
        elif n = 0.0 then
            0.0
        elif n = 1.0 then
            1.0
        else
            let mutable ans = 1.0

            for i = 1 to abs a do
                ans <- n * ans

            if a > 0 then ans else (1.0 / ans)

    // Fast exponentiation of a number
    let rec FastPow (n: float) (a: int) =
        // Checking the base and exponent. We don't want to multiply 0 or 1 by itself.
        if n = 0.0 && a = 0 then
            failwith "0 to the power of 0 is uncertainty"
        elif n = 0.0 then
            0.0
        elif n = 1 then
            1.0
        // We use the fast exponentiation algorithm
        else
            let ans =
                if a = 0 then
                    1.0
                else
                    let s = FastPow n (abs a / 2)
                    if a % 2 = 0 then s * s else s * s * n
            // The answer varies depending on the sign of the exponent.
            if a > 0 then ans else (1.0 / ans)


    // The difference between the maximum and minimum elements in the array
    let inline MinMaxFromArray (x: 'value []) =
        if x.Length = 0 then
            failwith "Array is empty"
        else
            let mutable min = x[0]
            let mutable max = x[0]

            // Iterating through the array and find the maximum and minimum elements
            for i = 0 to x.Length - 1 do
                if min > x[i] then min <- x[i]

                if max < x[i] then max <- x[i]

            max - min


    // Get an array of odd numbers between two given
    let OddNumbersArray (a: int) (b: int) =
        let min = if a < b then a else b
        let max = if a > b then a else b

        let oddsArray =
            [| for i in min + 1 .. max - 1 do
                   if i % 2 <> 0 then i |]

        oddsArray


    [<EntryPoint>]
    let main (argv: string array) =
        (*printfn "Choose what you want to do:\n1. SimplePow\n2. FastPow\n3. MinMaxFromArray\n4. OddNumbersArray"
        let var = Console.ReadLine() |> int

        match var with
        | 1 -> printfn $"Answer is: %A{SimplePow -2 -1}"
        | 2 -> printfn $"Answer is: %A{FastPow 2 -2}"
        | 3 -> printfn $"Answer is: %A{MinMaxFromArray [|12.1; 22; 13; 15; 110; 1|]}"
        | 4 -> printfn $"Answer is: %A{OddNumbersArray 0 10}"

        | _ -> printfn "Error"*)

        0
