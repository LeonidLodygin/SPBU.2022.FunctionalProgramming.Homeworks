module MyListHomework

// Type MyList: BubbleSort, QuickSort, Concatenation of two lists.
type MyList<'value> =
    | Cons of head: 'value * tail: MyList<'value>
    | Empty

/// The function receives two lists of type MyList as input and returns their union. (The second list is appended to the end of the first)
let rec concatenation (lst1: MyList<'value>) (lst2: MyList<'value>) =
    match lst1 with
    | Empty -> lst2
    | Cons (hd, tl) -> Cons(hd, concatenation tl lst2)

let TrainingList: MyList<int> =
    Cons(2, Cons(1, Cons(9, Cons(11, Cons(6, Cons(5, Cons(7, Cons(10, Cons(3, Cons(4, Cons(0, Empty)))))))))))

/// BubbleSort.(MyList type)
let BubbleSort (lst: MyList<'value>) =
    let rec bubble (lst: MyList<'value>) (changer: bool) (newList: MyList<'value>) =
        match lst, changer with
        | Empty, _ -> Empty
        | Cons (hd, Empty), false -> concatenation newList (Cons(hd, Empty))
        | Cons (hd, Empty), true -> bubble (concatenation newList (Cons(hd, Empty))) false Empty
        | Cons (hd1, Cons (hd2, tl)), _ ->
            if hd1 > hd2 then
                bubble (Cons(hd1, tl)) true (concatenation newList (Cons(hd2, Empty)))
            else
                bubble (Cons(hd2, tl)) (false || changer) (concatenation newList (Cons(hd1, Empty)))

    bubble lst false Empty

/// The function receives a list of type MyList and a value. Returns a list of elements less than the given value.
let rec MinList (lst: MyList<'value>) selected =
    let minList: MyList<'value> = Empty

    match lst with
    | Empty -> Empty
    | Cons (hd, Empty) ->
        if hd <= selected then
            concatenation minList (Cons(hd, Empty))
        else
            Empty
    | Cons (hd1, Cons (hd2, tl)) ->
        if hd1 <= selected then
            concatenation (concatenation minList (Cons(hd1, Empty))) (MinList(Cons(hd2, tl)) selected)
        else
            concatenation minList (MinList(Cons(hd2, tl)) selected)

/// The function receives a list of type MyList and a value. Returns a list of elements greater than the given value.
let rec MaxList (lst: MyList<'value>) selected =
    let maxList: MyList<'value> = Empty

    match lst with
    | Empty -> Empty
    | Cons (hd, Empty) ->
        if hd > selected then
            concatenation maxList (Cons(hd, Empty))
        else
            Empty
    | Cons (hd1, Cons (hd2, tl)) ->
        if hd1 > selected then
            concatenation (concatenation maxList (Cons(hd1, Empty))) (MaxList(Cons(hd2, tl)) selected)
        else
            concatenation maxList (MaxList(Cons(hd2, tl)) selected)

/// QuickSort.(MyList type)
let QuickSort (lst: MyList<'value>) =
    let rec sort (lst: MyList<'value>) =
        match lst with
        | Empty -> Empty
        | Cons (hd, Empty) -> Cons(hd, Empty)
        | Cons (hd1, Cons (hd2, tl)) ->
            concatenation
                (concatenation (sort (MinList(Cons(hd2, tl)) hd1)) (Cons(hd1, Empty)))
                (sort (MaxList(Cons(hd2, tl)) hd1))

    sort lst

