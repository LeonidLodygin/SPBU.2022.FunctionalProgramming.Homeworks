module MyListHomework

// Type MyList: BubbleSort, QuickSort, Concatenation of two lists.
type MyList<'Value> =
    | Cons of head: 'Value * tail: MyList<'Value>
    | Empty

let rec Length (lst: MyList<'Value>) =
    match lst with
    | Empty -> 0
    | Cons (hd, tl) -> 1 + (Length tl)

/// The function receives two lists of type MyList as input and returns their union. (The second list is appended to the end of the first)
let rec Concatenation (lst1: MyList<'Value>) (lst2: MyList<'Value>) =
    match lst1 with
    | Empty -> lst2
    | Cons (hd, tl) -> Cons(hd, Concatenation tl lst2)

/// The function receives a list of type MyList and returns the same list of built-in type List.
let rec MyListToList (lst: MyList<'Value>) =
    match lst with
    | Empty -> []
    | Cons (hd, tl) -> hd :: (MyListToList tl)

/// The function receives a list of built-in type List and returns the same list of type MyList.
let rec ListToMyList (lst: List<'Value>) =
    match lst with
    | [] -> Empty
    | hd :: tl -> Cons(hd, ListToMyList tl)

/// BubbleSort.(MyList type)
let BubbleSort (lst: MyList<'Value>) =
    let rec bubble (lst: MyList<'Value>) (changer: bool) (newList: MyList<'Value>) =
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

/// The function receives a list of type MyList, a value. Returns a cortege with two lists of elements less or greater than the given value.
let rec MinMaxList (lst: MyList<'Value>) selected =
    match lst with
    | Empty -> Empty, Empty
    | Cons (hd, tl) ->
        let tailMinMax = MinMaxList tl selected

        if hd <= selected then
            Cons(hd, fst tailMinMax), snd tailMinMax
        else
            fst tailMinMax, Cons(hd, snd tailMinMax)


/// QuickSort.(MyList type)
let QuickSort (lst: MyList<'Value>) =
    let rec sort (lst: MyList<'Value>) =
        match lst with
        | Empty -> Empty
        | Cons (hd, Empty) -> Cons(hd, Empty)
        | Cons (hd1, tl) ->
            let tailMinMax = MinMaxList tl hd1
            Concatenation(sort (fst tailMinMax)) (Cons(hd1, sort (snd tailMinMax)))

    sort lst
