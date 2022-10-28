module TreeHomework

open System.Collections.Generic
open MyListHomework

/// Tree with arbitrary number of children
type ArbitraryTree<'value> =
    | Leaf of value: 'value
    | Node of value: 'value * nodes: array<ArbitraryTree<'value>>

let rec ArrayWalker func param arr =
    let recur = ArrayWalker func

    match arr with
    | [||] -> param
    | arr -> recur (func param arr[0]) arr[1 .. arr.Length - 1]

let rec TreeWalker hell answer tree =
    match tree with
    | Leaf value -> hell answer value
    | Node (value, arr) ->
        let param = hell answer value
        ArrayWalker(TreeWalker hell) param arr

let MyListOfTree (tree: ArbitraryTree<'value>) =
    let ListFromTree (list: MyList<'value>) value = Cons(value, list)
    TreeWalker ListFromTree Empty tree

let NumOfDifferentValues (tree: ArbitraryTree<'value>) =
    let hash = HashSet<'value>()

    let HashSetFromTree (hashSet: HashSet<'value>) value =
        hashSet.Add(value) |> ignore
        hashSet

    (TreeWalker HashSetFromTree hash tree).Count
