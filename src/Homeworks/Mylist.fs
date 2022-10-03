module Homework2

open System.Runtime.CompilerServices


type MyList<'value> =
    | Cons of head: 'value * tail: MyList<'value>
    | Empty

let rec concat (lst1: MyList<'value>) (lst2: MyList<'value>) =
    match lst1 with
    | Empty -> lst2
    | Cons (hd, tl) -> Cons(hd, concat tl lst2)

let TrainingList: MyList<int> =
    Cons(2, Cons(1, Cons(9, Cons(11, Cons(6, Cons(5, Cons(7, Cons(10, Cons(3, Cons(4, Cons(0, Empty)))))))))))

let TrainingList2: MyList<int> =
    Cons(8, Cons(9, Cons(10, Empty)))

let TrainingList3: MyList<int> =
    Cons(5, Cons(2, Cons(10, Cons(1, Cons(4, Cons(6, Cons(6, Empty)))))))

let TrainingList4: MyList<int> =
    Cons(10, Cons(9, Cons(8, Cons(7, Cons(6, Cons(5, Cons(4, Cons(3, Cons(2, Cons(1, Cons(0, Empty)))))))))))

let BubbleSort (lst: MyList<'value>) =
    let rec bubble (lst: MyList<'value>) (changer: bool) (newList: MyList<'value>) =
        match lst, changer with
        | Empty, _ -> Empty
        | Cons (hd, Empty), false -> concat newList (Cons(hd, Empty))
        | Cons (hd, Empty), true -> bubble (concat newList (Cons(hd, Empty))) false Empty
        | Cons (hd1, Cons (hd2, tl)), _ ->
            if hd1 > hd2 then
                bubble (Cons(hd1, tl)) true (concat newList (Cons(hd2, Empty)))
            else
                bubble (Cons(hd2, tl)) (false || changer) (concat newList (Cons(hd1, Empty)))

    bubble lst false Empty

let rec MinList (lst: MyList<'value>) selected =
    let minList: MyList<'value> = Empty

    match lst with
    | Empty -> Empty
    | Cons (hd, Empty) ->
        if hd <= selected then
            concat minList (Cons(hd, Empty))
        else
            Empty
    | Cons (hd1, Cons (hd2, tl)) ->
        if hd1 <= selected then
            concat (concat minList (Cons(hd1, Empty))) (MinList(Cons(hd2, tl)) selected)
        else
            concat minList (MinList(Cons(hd2, tl)) selected)

let rec MaxList (lst: MyList<'value>) selected =
    let maxList: MyList<'value> = Empty

    match lst with
    | Empty -> Empty
    | Cons (hd, Empty) ->
        if hd > selected then
            concat maxList (Cons(hd, Empty))
        else
            Empty
    | Cons (hd1, Cons (hd2, tl)) ->
        if hd1 > selected then
            concat (concat maxList (Cons(hd1, Empty))) (MaxList(Cons(hd2, tl)) selected)
        else
            concat maxList (MaxList(Cons(hd2, tl)) selected)

let QuickSort (lst: MyList<'value>) =
    let rec sort (lst: MyList<'value>) =
        match lst with
        | Empty -> Empty
        | Cons (hd, Empty) -> Cons(hd, Empty)
        | Cons (hd1, Cons (hd2, tl)) ->
            concat (concat (sort (MinList(Cons(hd2, tl)) hd1)) (Cons(hd1, Empty))) (sort (MaxList(Cons(hd2, tl)) hd1))

    sort lst


// OOPList


type IList<'value> = interface end

type MyOOPNonEmptyList<'value> (head: 'value, tail: IList<'value>) =
    interface IList<'value>
    member this.Head = head
    member this.Tail = tail
type MyOOPEmptyList<'value>() =
    interface IList<'value>

let rec concatOOP (lst1: IList<'value>) (lst2: IList<'value>) =
    match lst1 with
    | :? MyOOPEmptyList<'value> -> lst2
    | :? MyOOPNonEmptyList<'value> as lst1 -> MyOOPNonEmptyList(lst1.Head, concatOOP lst1.Tail lst2)

let TrainingListOOP: IList<'value> =
    MyOOPNonEmptyList(2, MyOOPNonEmptyList(1, MyOOPNonEmptyList(9, MyOOPNonEmptyList(11, MyOOPNonEmptyList(6, MyOOPNonEmptyList(5, MyOOPNonEmptyList(7, MyOOPNonEmptyList(10, MyOOPNonEmptyList(3, MyOOPNonEmptyList(4, MyOOPNonEmptyList(0, MyOOPEmptyList())))))))))))


let takeOOPHead (lst: IList<'value>) : 'value =
    match lst with
    | :? MyOOPNonEmptyList<'value> as lst -> lst.Head


let takeOOPTail (lst: IList<'value>) : IList<'value> =
    match lst with
    | :? MyOOPNonEmptyList<'value> as lst ->
        if lst.Tail :? MyOOPEmptyList<'value> then
            MyOOPEmptyList()
        else
            lst.Tail


let BubbleSortOOP (lst: IList<'value>) =
    let rec bubble (lst: IList<'value>) (changer: bool) (newList: IList<'value>) =
        match lst, changer with
        | :? MyOOPEmptyList<'value>, _ -> MyOOPEmptyList() :> IList<'value>
        | :? MyOOPNonEmptyList<'value> as lst, _ ->
            if lst.Tail :? MyOOPEmptyList<'value> && changer = false then
                concatOOP newList (MyOOPNonEmptyList(lst.Head, MyOOPEmptyList()))
            elif lst.Tail :? MyOOPEmptyList<'value> && changer = true then
                bubble (concatOOP newList (MyOOPNonEmptyList(lst.Head, MyOOPEmptyList()))) false (MyOOPEmptyList())
            else
                if lst.Head > takeOOPHead lst.Tail then
                    bubble (MyOOPNonEmptyList(lst.Head, takeOOPTail lst.Tail)) true (concatOOP newList (MyOOPNonEmptyList(takeOOPHead lst.Tail, MyOOPEmptyList())))
                else
                    bubble (MyOOPNonEmptyList(takeOOPHead lst.Tail, takeOOPTail lst.Tail)) (false || changer) (concatOOP newList (MyOOPNonEmptyList(lst.Head, MyOOPEmptyList())))

    bubble lst false (MyOOPEmptyList())

let rec OOPListToMyList (lst:IList<'value>) =
    match lst with
    | :? MyOOPEmptyList<'value> -> Empty
    | :? MyOOPNonEmptyList<'value> as lst -> Cons(lst.Head, OOPListToMyList lst.Tail)

let rec MinOOPList (lst: IList<'value>) selected =
    let minList: IList<'value> = MyOOPEmptyList()

    match lst with
    | :? MyOOPEmptyList<'value> -> MyOOPEmptyList() :> IList<'value>
    | :? MyOOPNonEmptyList<'value> as lst ->
        if lst.Head <= selected then
            concatOOP minList (MyOOPNonEmptyList(lst.Head, MyOOPEmptyList()))
        else
            MyOOPEmptyList()
  //->  | Cons (hd1, Cons (hd2, tl)) ->
        if hd1 <= selected then
            concat (concat minList (Cons(hd1, Empty))) (MinList(Cons(hd2, tl)) selected)
        else
            concat minList (MinList(Cons(hd2, tl)) selected)

//printfn $"%A{OOPListToMyList (BubbleSortOOP TrainingListOOP)}"
