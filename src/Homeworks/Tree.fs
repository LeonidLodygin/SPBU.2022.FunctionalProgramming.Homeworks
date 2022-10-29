module TreeHomework

open System.Collections.Generic
open MyListHomework

/// Tree with arbitrary number of children
type ArbitraryTree<'value> =
    | Leaf of value: 'value
    | Node of value: 'value * nodes: array<ArbitraryTree<'value>>

let rec TreeWalker helper storage tree =
    match tree with
    | Leaf value -> helper storage value
    | Node (value, arr) -> Array.fold (TreeWalker helper) (helper storage value) arr

let MyListOfTree (tree: ArbitraryTree<'value>) =
    let ListFromTree (list: MyList<'value>) value = Cons(value, list)
    TreeWalker ListFromTree Empty tree

let NumOfDifferentValues (tree: ArbitraryTree<'value>) =
    let hash = HashSet<'value>()

    let HashSetFromTree (hashSet: HashSet<'value>) value =
        hashSet.Add(value) |> ignore
        hashSet

    (TreeWalker HashSetFromTree hash tree).Count
