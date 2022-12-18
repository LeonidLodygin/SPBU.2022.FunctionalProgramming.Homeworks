module TreeHomework

open System.Collections.Generic
open MyListHomework

/// Tree with arbitrary number of children
type ArbitraryTree<'Value> =
    | Leaf of value: 'Value
    | Node of value: 'Value * nodes: array<ArbitraryTree<'Value>>

let rec TreeWalker helper storage tree =
    match tree with
    | Leaf value -> helper storage value
    | Node (value, arr) -> Array.fold (TreeWalker helper) (helper storage value) arr

let MyListOfTree (tree: ArbitraryTree<'Value>) =
    let listFromTree (list: MyList<'Value>) value = Cons(value, list)
    TreeWalker listFromTree Empty tree

let NumOfDifferentValues (tree: ArbitraryTree<'Value>) =
    let hash = HashSet<'Value>()

    let hashSetFromTree (hashSet: HashSet<'Value>) value =
        hashSet.Add(value) |> ignore
        hashSet

    (TreeWalker hashSetFromTree hash tree).Count
