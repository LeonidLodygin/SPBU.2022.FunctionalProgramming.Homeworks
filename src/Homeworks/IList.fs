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
let rec Concatenation (lst1: IList<'value>) (lst2: IList<'value>) =
    match lst1 with
    | :? MyOOPEmptyList<'value> -> lst2
    | :? MyOOPNonEmptyList<'value> as lst1 -> MyOOPNonEmptyList(lst1.Head, Concatenation lst1.Tail lst2)
    | _ -> failwith "Use only MyOOPEmptyList or MyOOPNonEmptyList types"

/// The function receives OOPList and returns the value.
let GetHeadFromOOP (lst: IList<'value>) : 'value =
    match lst with
    | :? MyOOPNonEmptyList<'value> as lst -> lst.Head
    | _ -> failwith "Use only MyOOPEmptyList or MyOOPNonEmptyList types"

/// The function receives OOPList and returns the tail.
let GetTailFromOOP (lst: IList<'value>) : IList<'value> =
    match lst with
    | :? MyOOPNonEmptyList<'value> as lst ->
        if lst.Tail :? MyOOPEmptyList<'value> then
            MyOOPEmptyList()
        else
            lst.Tail
    | _ -> failwith "Use only MyOOPEmptyList or MyOOPNonEmptyList types"

/// BubbleSort.(IList type)
let BubbleSort (lst: IList<'value>) =
    let rec bubble (lst: IList<'value>) (changer: bool) (newList: IList<'value>) =
        match lst, changer with
        | :? MyOOPEmptyList<'value>, _ -> MyOOPEmptyList() :> IList<'value>
        | :? MyOOPNonEmptyList<'value> as lst, _ ->
            if lst.Tail :? MyOOPEmptyList<'value>
               && changer = false then
                Concatenation newList (MyOOPNonEmptyList(lst.Head, MyOOPEmptyList()))
            elif lst.Tail :? MyOOPEmptyList<'value>
                 && changer = true then
                bubble (Concatenation newList (MyOOPNonEmptyList(lst.Head, MyOOPEmptyList()))) false (MyOOPEmptyList())
            else if lst.Head > GetHeadFromOOP lst.Tail then
                bubble
                    (MyOOPNonEmptyList(lst.Head, GetTailFromOOP lst.Tail))
                    true
                    (Concatenation newList (MyOOPNonEmptyList(GetHeadFromOOP lst.Tail, MyOOPEmptyList())))
            else
                bubble
                    (MyOOPNonEmptyList(GetHeadFromOOP lst.Tail, GetTailFromOOP lst.Tail))
                    (false || changer)
                    (Concatenation newList (MyOOPNonEmptyList(lst.Head, MyOOPEmptyList())))
        | _ -> failwith "Use only MyOOPEmptyList or MyOOPNonEmptyList types"

    bubble lst false (MyOOPEmptyList())


/// The function receives a list of type IList and returns a list of type MyList.
let rec OOPListToMyList (lst: IList<'value>) =
    match lst with
    | :? MyOOPEmptyList<'value> -> Empty
    | :? MyOOPNonEmptyList<'value> as lst -> Cons(lst.Head, OOPListToMyList lst.Tail)
    | _ -> failwith "Use only MyOOPEmptyList or MyOOPNonEmptyList types"


/// The function receives a list of type IList and a value and bool variable(true - if we need a MinList, false - if we need a MaxList). Returns a list of elements less or greater than the given value.
let rec MinMaxList (lst: IList<'value>) selected bool =
    let newList: IList<'value> = MyOOPEmptyList()

    match lst with
    | :? MyOOPEmptyList<'value> -> MyOOPEmptyList() :> IList<'value>
    | :? MyOOPNonEmptyList<'value> as lst ->
        if lst.Tail :? MyOOPEmptyList<'value> then
            if (lst.Head <= selected) = bool then
                Concatenation newList (MyOOPNonEmptyList(lst.Head, MyOOPEmptyList()))
            else
                MyOOPEmptyList()
        else if (lst.Head <= selected) = bool then
            Concatenation
                (Concatenation newList (MyOOPNonEmptyList(lst.Head, MyOOPEmptyList())))
                (MinMaxList(MyOOPNonEmptyList(GetHeadFromOOP lst.Tail, GetTailFromOOP lst.Tail)) selected bool)
        else
            Concatenation
                newList
                (MinMaxList(MyOOPNonEmptyList(GetHeadFromOOP lst.Tail, GetTailFromOOP lst.Tail)) selected bool)
    | _ -> failwith "Use only MyOOPEmptyList or MyOOPNonEmptyList types"


///QuickSort.(IList type)
let QuickSort (lst: IList<'value>) =
    let rec sort (lst: IList<'value>) =
        match lst with
        | :? MyOOPEmptyList<'value> -> MyOOPEmptyList() :> IList<'value>
        | :? MyOOPNonEmptyList<'value> as lst ->
            if lst.Tail :? MyOOPEmptyList<'value> then
                MyOOPNonEmptyList(lst.Head, MyOOPEmptyList())
            else
                Concatenation
                    (sort (
                        MinMaxList(MyOOPNonEmptyList(GetHeadFromOOP lst.Tail, GetTailFromOOP lst.Tail)) lst.Head true
                    ))
                    (MyOOPNonEmptyList(
                        lst.Head,
                        sort (
                            MinMaxList
                                (MyOOPNonEmptyList(GetHeadFromOOP lst.Tail, GetTailFromOOP lst.Tail))
                                lst.Head
                                false
                        )
                    ))
        | _ -> failwith "Use only MyOOPEmptyList or MyOOPNonEmptyList types"

    sort lst
