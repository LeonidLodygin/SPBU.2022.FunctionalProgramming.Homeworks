module TreeHomework

open System.Collections.Generic
open MyListHomework

/// Tree with arbitrary number of children
type ArbitraryTree<'value> =
    | Node of value: 'value * nodes: array<ArbitraryTree<'value>>
    | Leaf of value: 'value

/// Function receives a tree of type ArbitraryTree, iterates through it and sends the values from the nodes and leaves to the HashMap
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

/// Function receives a tree of type ArbitraryTree and returns list of type MyList of values from nodes and leaves
let rec MyListOfTree (tree: ArbitraryTree<'value>) =
    match tree with
    | Leaf value -> Cons(value, Empty)
    | Node (value, array) ->
        let rec ArrayOfTreesToMyList (array: array<ArbitraryTree<'value>>) =
            match array with
            | [||] -> Empty
            | array -> Concatenation(MyListOfTree array[0]) (ArrayOfTreesToMyList array[1 .. array.Length - 1])

        Cons(value, ArrayOfTreesToMyList array)
