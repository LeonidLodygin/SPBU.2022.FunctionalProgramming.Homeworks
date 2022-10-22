module TreeHomework

open System.Collections.Generic
open MyListHomework

type ArbitraryTree<'value> =
    | Node of value: 'value * nodes: array<ArbitraryTree<'value>>
    | Leaf of value: 'value

let NumOfDifferentValues (tree: ArbitraryTree<'value>) =
    let hashSet = new HashSet<'value>()

    let rec Counter tree =
        match tree with
        | Leaf value -> hashSet.Add(value) |> ignore
        | Node (value, array) ->
            for i in array do
                Counter i

            hashSet.Add(value) |> ignore

    Counter(tree)
    hashSet.Count

let rec MyListOfTree (tree: ArbitraryTree<'value>) =
    match tree with
    | Leaf value -> Cons(value, Empty)
    | Node (value, array) ->
        let rec ArrayOfTreesToMyList (array: array<ArbitraryTree<'value>>) =
            match array with
            | [||] -> Empty
            | array -> Concatenation(MyListOfTree array[0]) (ArrayOfTreesToMyList array[1 .. array.Length - 1])

        Cons(value, ArrayOfTreesToMyList array)
