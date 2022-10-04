module Homework2

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

let TrainingList2: MyList<int> =
    Cons(8, Cons(9, Cons(10, Empty)))

let TrainingList3: MyList<int> =
    Cons(5, Cons(2, Cons(10, Cons(1, Cons(4, Cons(6, Cons(6, Empty)))))))

let TrainingList4: MyList<int> =
    Cons(10, Cons(9, Cons(8, Cons(7, Cons(6, Cons(5, Cons(4, Cons(3, Cons(2, Cons(1, Cons(0, Empty)))))))))))

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
            concatenation (concatenation (sort (MinList(Cons(hd2, tl)) hd1)) (Cons(hd1, Empty))) (sort (MaxList(Cons(hd2, tl)) hd1))

    sort lst


// Type IList: BubbleSort, QuickSort, Concatenation of two lists.


type IList<'value> = interface end

/// OOPList with some value and tail
type MyOOPNonEmptyList<'value> (head: 'value, tail: IList<'value>) =
    interface IList<'value>
    member this.Head = head
    member this.Tail = tail

/// Empty OOPList
type MyOOPEmptyList<'value>() =
    interface IList<'value>

/// The function receives two lists of type IList as input and returns their union. (The second list is appended to the end of the first)
let rec concatOOP (lst1: IList<'value>) (lst2: IList<'value>) =
    match lst1 with
    | :? MyOOPEmptyList<'value> -> lst2
    | :? MyOOPNonEmptyList<'value> as lst1 -> MyOOPNonEmptyList(lst1.Head, concatOOP lst1.Tail lst2)
    | _ -> failwith "Use only MyOOPEmptyList or MyOOPNonEmptyList types"

let TrainingListOOP: IList<int> =
    MyOOPNonEmptyList(2, MyOOPNonEmptyList(1, MyOOPNonEmptyList(9, MyOOPNonEmptyList(11, MyOOPNonEmptyList(6, MyOOPNonEmptyList(5, MyOOPNonEmptyList(7, MyOOPNonEmptyList(10, MyOOPNonEmptyList(3, MyOOPNonEmptyList(4, MyOOPNonEmptyList(0, MyOOPEmptyList())))))))))))

let TrainingListOOP2: IList<int> =
    MyOOPNonEmptyList(2, MyOOPEmptyList())


/// The function receives OOPList and returns the value.
let OOPHead (lst: IList<'value>) : 'value =
    match lst with
    | :? MyOOPNonEmptyList<'value> as lst -> lst.Head
    | _ -> failwith "Use only MyOOPEmptyList or MyOOPNonEmptyList types"

/// The function receives OOPList and returns the tail.
let OOPTail (lst: IList<'value>) : IList<'value> =
    match lst with
    | :? MyOOPNonEmptyList<'value> as lst ->
        if lst.Tail :? MyOOPEmptyList<'value> then
            MyOOPEmptyList()
        else
            lst.Tail
    | _ -> failwith "Use only MyOOPEmptyList or MyOOPNonEmptyList types"

/// BubbleSort.(IList type)
let bubbleSortOOP (lst: IList<'value>) =
    let rec bubble (lst: IList<'value>) (changer: bool) (newList: IList<'value>) =
        match lst, changer with
        | :? MyOOPEmptyList<'value>, _ -> MyOOPEmptyList() :> IList<'value>
        | :? MyOOPNonEmptyList<'value> as lst, _ ->
            if lst.Tail :? MyOOPEmptyList<'value> && changer = false then
                concatOOP newList (MyOOPNonEmptyList(lst.Head, MyOOPEmptyList()))
            elif lst.Tail :? MyOOPEmptyList<'value> && changer = true then
                bubble (concatOOP newList (MyOOPNonEmptyList(lst.Head, MyOOPEmptyList()))) false (MyOOPEmptyList())
            else
                if lst.Head > OOPHead lst.Tail then
                    bubble (MyOOPNonEmptyList(lst.Head, OOPTail lst.Tail)) true (concatOOP newList (MyOOPNonEmptyList(OOPHead lst.Tail, MyOOPEmptyList())))
                else
                    bubble (MyOOPNonEmptyList(OOPHead lst.Tail, OOPTail lst.Tail)) (false || changer) (concatOOP newList (MyOOPNonEmptyList(lst.Head, MyOOPEmptyList())))
        | _ -> failwith "Use only MyOOPEmptyList or MyOOPNonEmptyList types"
    bubble lst false (MyOOPEmptyList())


/// The function receives a list of type IList and returns a list of type MyList.
let rec OOPListToMyList (lst:IList<'value>) =
    match lst with
    | :? MyOOPEmptyList<'value> -> Empty
    | :? MyOOPNonEmptyList<'value> as lst -> Cons(lst.Head, OOPListToMyList lst.Tail)
    | _ -> failwith "Use only MyOOPEmptyList or MyOOPNonEmptyList types"


/// The function receives a list of type IList and a value. Returns a list of elements less than the given value.
let rec minOOPList (lst: IList<'value>) selected =
    let minList: IList<'value> = MyOOPEmptyList()

    match lst with
    | :? MyOOPEmptyList<'value> -> MyOOPEmptyList() :> IList<'value>
    | :? MyOOPNonEmptyList<'value> as lst ->
        if lst.Tail :? MyOOPEmptyList<'value> then
            if lst.Head <= selected then
                concatOOP minList (MyOOPNonEmptyList(lst.Head, MyOOPEmptyList()))
            else
                MyOOPEmptyList()
        else
            if lst.Head <= selected then
                concatOOP (concatOOP minList (MyOOPNonEmptyList(lst.Head, MyOOPEmptyList()))) (minOOPList(MyOOPNonEmptyList(OOPHead lst.Tail, OOPTail lst.Tail)) selected)
            else
                concatOOP minList (minOOPList(MyOOPNonEmptyList(OOPHead lst.Tail, OOPTail lst.Tail)) selected)
    | _ ->  failwith "Use only MyOOPEmptyList or MyOOPNonEmptyList types"


/// The function receives a list of type IList and a value. Returns a list of elements greater than the given value.
let rec maxOOPList (lst: IList<'value>) selected =
    let maxList: IList<'value> = MyOOPEmptyList()

    match lst with
    | :? MyOOPEmptyList<'value> -> MyOOPEmptyList() :> IList<'value>
    | :? MyOOPNonEmptyList<'value> as lst ->
        if lst.Tail :? MyOOPEmptyList<'value> then
            if lst.Head > selected then
                concatOOP maxList (MyOOPNonEmptyList(lst.Head, MyOOPEmptyList()))
            else
                MyOOPEmptyList()
        else
            if lst.Head > selected then
                concatOOP (concatOOP maxList (MyOOPNonEmptyList(lst.Head, MyOOPEmptyList()))) (maxOOPList(MyOOPNonEmptyList(OOPHead lst.Tail, OOPTail lst.Tail)) selected)
            else
                concatOOP maxList (maxOOPList (MyOOPNonEmptyList(OOPHead lst.Tail, OOPTail lst.Tail)) selected)
    | _ ->  failwith "Use only MyOOPEmptyList or MyOOPNonEmptyList types"


///QuickSort.(IList type)
let quickSortOOP (lst: IList<'value>) =
    let rec sort (lst: IList<'value>) =
        match lst with
        | :? MyOOPEmptyList<'value> -> MyOOPEmptyList() :> IList<'value>
        | :? MyOOPNonEmptyList<'value> as lst ->
            if lst.Tail :? MyOOPEmptyList<'value> then
                MyOOPNonEmptyList(lst.Head, MyOOPEmptyList())
            else
                concatOOP (concatOOP (sort (minOOPList(MyOOPNonEmptyList(OOPHead lst.Tail, OOPTail lst.Tail)) lst.Head)) (MyOOPNonEmptyList(lst.Head, MyOOPEmptyList()))) (sort (maxOOPList(MyOOPNonEmptyList(OOPHead lst.Tail, OOPTail lst.Tail)) lst.Head))
        | _ -> failwith "Use only MyOOPEmptyList or MyOOPNonEmptyList types"
    sort lst

