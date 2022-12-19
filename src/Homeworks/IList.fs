module IListHomework

open MyListHomework

// Type IList: BubbleSort, QuickSort, Concatenation of two lists.

type IList<'Value> =
    interface
    end

/// OOPList with some value and tail
type List<'Value>(head: 'Value, tail: IList<'Value>) =
    interface IList<'Value>
    member this.Head = head
    member this.Tail = tail

/// Empty OOPList
type EmptyList<'Value>() =
    interface IList<'Value>

/// The function receives two lists of type IList as input and returns their union. (The second list is appended to the end of the first)
let rec Concatenation (lst1: IList<'Value>) (lst2: IList<'Value>) =
    match lst1 with
    | :? EmptyList<'Value> -> lst2
    | :? List<'Value> as lst1 -> List(lst1.Head, Concatenation lst1.Tail lst2)
    | _ -> failwith $"This type %A{lst1.GetType()} is not allowed to be used. Use only EmptyList or List types"

/// The function receives OOPList and returns the value.
let Head (lst: IList<'Value>) : 'Value =
    match lst with
    | :? List<'Value> as lst -> lst.Head
    | _ -> failwith $"This type %A{lst.GetType()} is not allowed to be used Use only EmptyList or List types"

/// The function receives OOPList and returns the tail.
let Tail (lst: IList<'Value>) : IList<'Value> =
    match lst with
    | :? List<'Value> as lst ->
        if lst.Tail :? EmptyList<'Value> then
            EmptyList()
        else
            lst.Tail
    | _ -> failwith $"This type %A{lst.GetType()} is not allowed to be used Use only EmptyList or List types"

/// BubbleSort.(IList type)
let BubbleSort (lst: IList<'Value>) =
    let rec bubble (lst: IList<'Value>) (changer: bool) (newList: IList<'Value>) =
        match lst with
        | :? EmptyList<'Value> -> EmptyList() :> IList<'Value>
        | :? List<'Value> as lst ->
            if lst.Tail :? EmptyList<'Value> && not changer then
                Concatenation newList (List(lst.Head, EmptyList()))
            elif lst.Tail :? EmptyList<'Value> && changer then
                bubble (Concatenation newList (List(lst.Head, EmptyList()))) false (EmptyList())
            else if lst.Head > Head lst.Tail then
                bubble (List(lst.Head, Tail lst.Tail)) true (Concatenation newList (List(Head lst.Tail, EmptyList())))
            else
                bubble
                    (List(Head lst.Tail, Tail lst.Tail))
                    changer
                    (Concatenation newList (List(lst.Head, EmptyList())))
        | _ -> failwith $"This type %A{lst.GetType()} is not allowed to be used Use only EmptyList or List types"

    bubble lst false (EmptyList())


/// The function receives a list of type IList and returns a list of type MyList.
let rec OOPListToMyList (lst: IList<'Value>) =
    match lst with
    | :? EmptyList<'Value> -> Empty
    | :? List<'Value> as lst -> Cons(lst.Head, OOPListToMyList lst.Tail)
    | _ -> failwith $"This type %A{lst.GetType()} is not allowed to be used. Use only EmptyList or List types"
/// The function receives a list of type MyList and returns a list of type IList.
let rec MyListToOOPList (lst: MyList<'Value>) =
    match lst with
    | Empty -> EmptyList() :> IList<'Value>
    | Cons (hd, tl) -> List(hd, MyListToOOPList tl)


/// The function receives a list of type IList and a value. Returns a cortege with two lists of elements less or greater than the given value.
let rec MinMaxList (lst: IList<'Value>) selected =

    match lst with
    | :? EmptyList<'Value> -> EmptyList() :> IList<'Value>, EmptyList() :> IList<'Value>
    | :? List<'Value> as lst ->
        let tailMinMax = MinMaxList lst.Tail selected

        if lst.Head <= selected then
            List(lst.Head, fst tailMinMax), snd tailMinMax
        else
            fst tailMinMax, List(lst.Head, snd tailMinMax)
    | _ -> failwith $"This type %A{lst.GetType()} is not allowed to be used. Use only EmptyList or List types"


///QuickSort.(IList type)
let QuickSort (lst: IList<'Value>) =
    let rec sort (lst: IList<'Value>) =
        match lst with
        | :? EmptyList<'Value> -> EmptyList() :> IList<'Value>
        | :? List<'Value> as lst ->
            if lst.Tail :? EmptyList<'Value> then
                List(lst.Head, EmptyList())
            else
                let tailMinMax = MinMaxList(List(Head lst.Tail, Tail lst.Tail)) lst.Head
                Concatenation(sort (fst tailMinMax)) (List(lst.Head, sort (snd tailMinMax)))
        | _ -> failwith $"This type %A{lst.GetType()} is not allowed to be used. Use only EmptyList or List types"

    sort lst
