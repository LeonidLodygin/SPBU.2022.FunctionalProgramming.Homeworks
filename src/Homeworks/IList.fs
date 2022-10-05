module IListHomework

open MyListHomework


// Type IList: BubbleSort, QuickSort, Concatenation of two lists.


type IList<'value> =
    interface
    end

/// OOPList with some value and tail
type MyOOPNonEmptyList<'value>(head: 'value, tail: IList<'value>) =
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
    MyOOPNonEmptyList(
        2,
        MyOOPNonEmptyList(
            1,
            MyOOPNonEmptyList(
                9,
                MyOOPNonEmptyList(
                    11,
                    MyOOPNonEmptyList(
                        6,
                        MyOOPNonEmptyList(
                            5,
                            MyOOPNonEmptyList(
                                7,
                                MyOOPNonEmptyList(
                                    10,
                                    MyOOPNonEmptyList(3, MyOOPNonEmptyList(4, MyOOPNonEmptyList(0, MyOOPEmptyList())))
                                )
                            )
                        )
                    )
                )
            )
        )
    )


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
            if lst.Tail :? MyOOPEmptyList<'value>
               && changer = false then
                concatOOP newList (MyOOPNonEmptyList(lst.Head, MyOOPEmptyList()))
            elif lst.Tail :? MyOOPEmptyList<'value>
                 && changer = true then
                bubble (concatOOP newList (MyOOPNonEmptyList(lst.Head, MyOOPEmptyList()))) false (MyOOPEmptyList())
            else if lst.Head > OOPHead lst.Tail then
                bubble
                    (MyOOPNonEmptyList(lst.Head, OOPTail lst.Tail))
                    true
                    (concatOOP newList (MyOOPNonEmptyList(OOPHead lst.Tail, MyOOPEmptyList())))
            else
                bubble
                    (MyOOPNonEmptyList(OOPHead lst.Tail, OOPTail lst.Tail))
                    (false || changer)
                    (concatOOP newList (MyOOPNonEmptyList(lst.Head, MyOOPEmptyList())))
        | _ -> failwith "Use only MyOOPEmptyList or MyOOPNonEmptyList types"

    bubble lst false (MyOOPEmptyList())


/// The function receives a list of type IList and returns a list of type MyList.
let rec OOPListToMyList (lst: IList<'value>) =
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
        else if lst.Head <= selected then
            concatOOP
                (concatOOP minList (MyOOPNonEmptyList(lst.Head, MyOOPEmptyList())))
                (minOOPList (MyOOPNonEmptyList(OOPHead lst.Tail, OOPTail lst.Tail)) selected)
        else
            concatOOP minList (minOOPList (MyOOPNonEmptyList(OOPHead lst.Tail, OOPTail lst.Tail)) selected)
    | _ -> failwith "Use only MyOOPEmptyList or MyOOPNonEmptyList types"


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
        else if lst.Head > selected then
            concatOOP
                (concatOOP maxList (MyOOPNonEmptyList(lst.Head, MyOOPEmptyList())))
                (maxOOPList (MyOOPNonEmptyList(OOPHead lst.Tail, OOPTail lst.Tail)) selected)
        else
            concatOOP maxList (maxOOPList (MyOOPNonEmptyList(OOPHead lst.Tail, OOPTail lst.Tail)) selected)
    | _ -> failwith "Use only MyOOPEmptyList or MyOOPNonEmptyList types"


///QuickSort.(IList type)
let quickSortOOP (lst: IList<'value>) =
    let rec sort (lst: IList<'value>) =
        match lst with
        | :? MyOOPEmptyList<'value> -> MyOOPEmptyList() :> IList<'value>
        | :? MyOOPNonEmptyList<'value> as lst ->
            if lst.Tail :? MyOOPEmptyList<'value> then
                MyOOPNonEmptyList(lst.Head, MyOOPEmptyList())
            else
                concatOOP
                    (concatOOP
                        (sort (minOOPList (MyOOPNonEmptyList(OOPHead lst.Tail, OOPTail lst.Tail)) lst.Head))
                        (MyOOPNonEmptyList(lst.Head, MyOOPEmptyList())))
                    (sort (maxOOPList (MyOOPNonEmptyList(OOPHead lst.Tail, OOPTail lst.Tail)) lst.Head))
        | _ -> failwith "Use only MyOOPEmptyList or MyOOPNonEmptyList types"

    sort lst
