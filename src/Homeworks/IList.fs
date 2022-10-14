module IListHomework

open MyListHomework

// Type IList: BubbleSort, QuickSort, Concatenation of two lists.

type IList<'value> =
    interface
    end

/// OOPList with some value and tail
type List<'value>(head: 'value, tail: IList<'value>) =
    interface IList<'value>
    member this.Head = head
    member this.Tail = tail

/// Empty OOPList
type EmptyList<'value>() =
    interface IList<'value>

/// The function receives two lists of type IList as input and returns their union. (The second list is appended to the end of the first)
let rec Concatenation (lst1: IList<'value>) (lst2: IList<'value>) =
    match lst1 with
    | :? EmptyList<'value> -> lst2
    | :? List<'value> as lst1 -> List(lst1.Head, Concatenation lst1.Tail lst2)
    | _ -> failwith "Use only EmptyList or List types"

/// The function receives OOPList and returns the value.
let Head (lst: IList<'value>) : 'value =
    match lst with
    | :? List<'value> as lst -> lst.Head
    | _ -> failwith "Use only EmptyList or List types"

/// The function receives OOPList and returns the tail.
let Tail (lst: IList<'value>) : IList<'value> =
    match lst with
    | :? List<'value> as lst ->
        if lst.Tail :? EmptyList<'value> then
            EmptyList()
        else
            lst.Tail
    | _ -> failwith "Use only EmptyList or List types"

/// BubbleSort.(IList type)
let BubbleSort (lst: IList<'value>) =
    let rec bubble (lst: IList<'value>) (changer: bool) (newList: IList<'value>) =
        match lst with
        | :? EmptyList<'value> -> EmptyList() :> IList<'value>
        | :? List<'value> as lst ->
            if lst.Tail :? EmptyList<'value> && not changer then
                Concatenation newList (List(lst.Head, EmptyList()))
            elif lst.Tail :? EmptyList<'value> && changer then
                bubble (Concatenation newList (List(lst.Head, EmptyList()))) false (EmptyList())
            else if lst.Head > Head lst.Tail then
                bubble (List(lst.Head, Tail lst.Tail)) true (Concatenation newList (List(Head lst.Tail, EmptyList())))
            else
                bubble
                    (List(Head lst.Tail, Tail lst.Tail))
                    changer
                    (Concatenation newList (List(lst.Head, EmptyList())))
        | _ -> failwith "Use only EmptyList or List types"

    bubble lst false (EmptyList())


/// The function receives a list of type IList and returns a list of type MyList.
let rec OOPListToMyList (lst: IList<'value>) =
    match lst with
    | :? EmptyList<'value> -> Empty
    | :? List<'value> as lst -> Cons(lst.Head, OOPListToMyList lst.Tail)
    | _ -> failwith "Use only EmptyList or List types"
/// The function receives a list of type MyList and returns a list of type IList.
let rec MyListToOOPList (lst: MyList<'value>) =
    match lst with
    | Empty -> EmptyList() :> IList<'value>
    | Cons (hd, tl) -> List(hd, MyListToOOPList tl)


/// The function receives a list of type IList and a value and bool variable(true - if we need a MinList, false - if we need a MaxList). Returns a list of elements less or greater than the given value.
let rec MinMaxList (lst: IList<'value>) selected bool =
    let newList: IList<'value> = EmptyList()

    match lst with
    | :? EmptyList<'value> -> EmptyList() :> IList<'value>
    | :? List<'value> as lst ->
        if lst.Tail :? EmptyList<'value> then
            if (lst.Head <= selected) = bool then
                Concatenation newList (List(lst.Head, EmptyList()))
            else
                EmptyList()
        else if (lst.Head <= selected) = bool then
            Concatenation
                (Concatenation newList (List(lst.Head, EmptyList())))
                (MinMaxList(List(Head lst.Tail, Tail lst.Tail)) selected bool)
        else
            Concatenation newList (MinMaxList(List(Head lst.Tail, Tail lst.Tail)) selected bool)
    | _ -> failwith "Use only EmptyList or List types"


///QuickSort.(IList type)
let QuickSort (lst: IList<'value>) =
    let rec sort (lst: IList<'value>) =
        match lst with
        | :? EmptyList<'value> -> EmptyList() :> IList<'value>
        | :? List<'value> as lst ->
            if lst.Tail :? EmptyList<'value> then
                List(lst.Head, EmptyList())
            else
                Concatenation
                    (sort (MinMaxList(List(Head lst.Tail, Tail lst.Tail)) lst.Head true))
                    (List(lst.Head, sort (MinMaxList(List(Head lst.Tail, Tail lst.Tail)) lst.Head false)))
        | _ -> failwith "Use only EmptyList or List types"

    sort lst
