module MyListHomework

// Type MyList: BubbleSort, QuickSort, Concatenation of two lists.
type MyList<'value> =
    | Cons of head: 'value * tail: MyList<'value>
    | Empty

/// The function receives two lists of type MyList as input and returns their union. (The second list is appended to the end of the first)
let rec Concatenation (lst1: MyList<'value>) (lst2: MyList<'value>) =
    match lst1 with
    | Empty -> lst2
    | Cons (hd, tl) -> Cons(hd, Concatenation tl lst2)


/// BubbleSort.(MyList type)
let BubbleSort (lst: MyList<'value>) =
    let rec bubble (lst: MyList<'value>) (changer: bool) (newList: MyList<'value>) =
        match lst, changer with
        | Empty, _ -> Empty
        | Cons (hd, Empty), false -> Concatenation newList (Cons(hd, Empty))
        | Cons (hd, Empty), true -> bubble (Concatenation newList (Cons(hd, Empty))) false Empty
        | Cons (hd1, Cons (hd2, tl)), _ ->
            if hd1 > hd2 then
                bubble (Cons(hd1, tl)) true (Concatenation newList (Cons(hd2, Empty)))
            else
                bubble (Cons(hd2, tl)) changer (Concatenation newList (Cons(hd1, Empty)))

    bubble lst false Empty

/// The function receives a list of type MyList, a value and bool variable(true - if we need a MinList, false - if we need a MaxList). Returns a list of elements less or greater than the given value.
let rec MinMaxList (lst: MyList<'value>) selected bool =
    let newList: MyList<'value> = Empty

    match lst with
    | Empty -> Empty
    | Cons (hd, Empty) ->
        if (hd <= selected) = bool then
            Concatenation newList (Cons(hd, Empty))
        else
            Empty
    | Cons (hd1, Cons (hd2, tl)) ->
        if (hd1 <= selected) = bool then
            Concatenation newList (Cons(hd1, (MinMaxList(Cons(hd2, tl)) selected bool)))
        else
            Concatenation newList (MinMaxList(Cons(hd2, tl)) selected bool)

/// QuickSort.(MyList type)
let QuickSort (lst: MyList<'value>) =
    let rec sort (lst: MyList<'value>) =
        match lst with
        | Empty -> Empty
        | Cons (hd, Empty) -> Cons(hd, Empty)
        | Cons (hd1, Cons (hd2, tl)) ->
            Concatenation
                (sort (MinMaxList(Cons(hd2, tl)) hd1 true))
                (Cons(hd1, sort (MinMaxList(Cons(hd2, tl)) hd1 false)))

    sort lst
