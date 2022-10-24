module TreeHomework

open System.Collections.Generic
open MyListHomework

/// Tree with arbitrary number of children
type ArbitraryTree<'value> =
    | Node of value: 'value * nodes: array<ArbitraryTree<'value>>
    | Leaf of value: 'value

/// The function iterates through the tree and returns a tuple consisting of a list of type MyList and the number of different elements in the tree
let rec TreeWalker (tree: ArbitraryTree<'value>) (hashSet: HashSet<'value>) =
    match tree with
    | Leaf value ->
        hashSet.Add(value) |> ignore
        Cons(value, Empty), hashSet
    | Node (value, array) ->
        hashSet.Add(value) |> ignore

        let rec ArrayOfTreesToMyList (array: array<ArbitraryTree<'value>>) (hashSet: HashSet<'value>) =
            match array with
            | [||] -> Empty
            | array ->
                Concatenation
                    (fst (TreeWalker array[0] hashSet))
                    (ArrayOfTreesToMyList array[1 .. array.Length - 1] hashSet)

        Cons(value, ArrayOfTreesToMyList array hashSet), hashSet

let NumOfDifferentValues (tree: ArbitraryTree<'value>) =
    let hashSet = new HashSet<'value>()
    (snd (TreeWalker tree hashSet)).Count

let MyListOfTree (tree: ArbitraryTree<'value>) =
    let hashSet = new HashSet<'value>()
    fst (TreeWalker tree hashSet)
