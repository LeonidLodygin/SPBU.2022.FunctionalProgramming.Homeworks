module TreeHomework
open System.Collections.Generic
open MyListHomework

type ArbitraryTree<'value> =
    | Node of value : 'value * nodes : array<ArbitraryTree<'value>>
    | Leaf of value : 'value

let TrainingTree: ArbitraryTree<int> = Node(12, [|Node(1, [|Leaf(12)|]); Node(3, [|Leaf(12)|]); Node(10, [|Node(4, [|Leaf(12)|])|]); Node(15, [|Leaf(12);Leaf(1)|])|])

let NumOfDifferentValues (tree:ArbitraryTree<'value>) =
    let hashSet = new HashSet<'value>()
    let rec Counter tree =
        match tree with
        | Leaf value ->
            hashSet.Add(value) |> ignore
        | Node (value, array) ->
            for i in array do
               Counter i
            hashSet.Add(value) |> ignore
    Counter(tree)
    hashSet.Count

let rec MyListOfTree (tree:ArbitraryTree<'value>) =
    match tree with
    | Leaf value ->
        Cons(value, Empty)
    | Node (value, array) ->
        let rec ArrayOfTreesToMyList (array: array<ArbitraryTree<'value>>) =
            match array with
            | [||] -> Empty
            | array -> Concatenation (MyListOfTree array[0]) (ArrayOfTreesToMyList array[1 .. array.Length - 1])
        Cons(value, ArrayOfTreesToMyList array)
