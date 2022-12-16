﻿module SparseVector

open System
open Microsoft.FSharp.Core

let First (x, _, _) = x
let Second (_, x, _) = x
let Third (_, _, x) = x

type Vector<'value> =
    struct
        val Memory: array<'value option>
        val Head: uint
        val Length: uint

        new(memory, head, length) =
            { Memory = memory
              Head = head
              Length = length }
    end

type BinaryTree<'value> =
    | Node of BinaryTree<'value> * BinaryTree<'value>
    | Leaf of 'value
    | None

let ClosestDegreeOf2 (columns: uint) (lines: uint) =
    uint (
        2.0
        ** Math.Ceiling(Math.Log(float (max columns lines), 2.0))
    )

let Separator (vec: Vector<'value>) =
    Vector(vec.Memory, vec.Head, vec.Length / 2u), Vector(vec.Memory, vec.Head + vec.Length / 2u, vec.Length / 2u)

let NoneDestroyer (tree: BinaryTree<'value>) =
    match tree with
    | Leaf value -> Leaf value
    | Node (None, None) -> None
    | _ -> tree

let Transformer (arr: array<'value option>) =
    let virtualLength = ClosestDegreeOf2(uint arr.Length) 0u
    let virtualVec = Vector(arr, 0u, virtualLength)

    let rec helper (virtualVec: Vector<'value>) =
        if virtualVec.Head >= uint arr.Length then
            None
        elif virtualVec.Length = 1u then
            let uHead =
                try
                    Convert.ToInt32(virtualVec.Head)
                with
                | :? OverflowException -> failwith $"%A{virtualVec.Head} is outside the range of the Int32 type."

            if virtualVec.Memory[uHead] = Option.None then
                None
            else
                Leaf (virtualVec.Memory[uHead]).Value
        else
            let leftTree = helper (fst (Separator virtualVec))
            let rightTree = helper (snd (Separator virtualVec))
            Node(leftTree, rightTree) |> NoneDestroyer

    if arr.Length = 0 then
        None
    else
        helper virtualVec

let ListVecSeparator (list: List<uint * 'a>) size =
    let rec helper list leftList rightList =
        match list with
        | [] -> leftList, rightList
        | (x, y) :: tl ->
            if x < size / 2u then
                helper tl ((x, y) :: leftList) rightList
            else
                helper tl leftList ((x - size / 2u, y) :: rightList)

    helper list [] []

let VecFromList (list: List<uint * 'a>) size =
    let virtualLength = ClosestDegreeOf2 size 0u

    let rec helper list virtualLength =
        if virtualLength = 0u then
            BinaryTree.None
        elif virtualLength = 1u
             && not (List.isEmpty list)
             && (fst list.Head < size) then
            BinaryTree.Leaf(snd list.Head)
        elif virtualLength = 1u
             && ((List.isEmpty list) || (fst list.Head >= size)) then
            BinaryTree.None
        else
            let lists = ListVecSeparator list virtualLength

            BinaryTree.Node(helper (fst lists) (virtualLength / 2u), helper (snd lists) (virtualLength / 2u))
            |> NoneDestroyer

    helper list virtualLength

type SparseVector<'value when 'value: equality> =
    val Memory: BinaryTree<'value>
    val Length: uint

    new(arr) =
        { Memory = Transformer arr
          Length = uint arr.Length }

    new(tree, length) = { Memory = tree; Length = length }

    new(list, length) =
        { Memory = VecFromList list length
          Length = length }

    member this.Item
        with get i =
            if i >= this.Length then
                failwith $"Index %A{i} is out of range."
            else
                let virtualLength = ClosestDegreeOf2 this.Length 0u

                let rec GetElementByIndex tree length index =
                    match tree with
                    | None -> Option.None
                    | Leaf value -> Some value
                    | Node (left, right) ->
                        if index < length / 2u then
                            GetElementByIndex left (length / 2u) index
                        else
                            GetElementByIndex right (length / 2u) (index - length / 2u)

                GetElementByIndex this.Memory virtualLength i

    member this.isEmpty =
        match this.Memory with
        | BinaryTree.None -> true
        | _ -> false

let NoneOrValue x =
    match x with
    | Option.None -> None
    | Some value -> Leaf value

let FAddVector
    (func: 'a option -> 'b option -> 'c option)
    (vec1: SparseVector<'a>)
    (vec2: SparseVector<'b>)
    : SparseVector<'c> =
    let rec Helper (tree1: BinaryTree<'a>) (tree2: BinaryTree<'b>) : BinaryTree<'c> =
        match tree1, tree2 with
        | None, None -> BinaryTree.None
        | Leaf value1, Leaf value2 -> func (Some value1) (Some value2) |> NoneOrValue
        | None, Leaf value -> func Option.None (Some value) |> NoneOrValue
        | Leaf value, None -> func (Some value) Option.None |> NoneOrValue
        | None, Node (left, right) ->
            Node(Helper None left, Helper None right)
            |> NoneDestroyer
        | Node (left, right), None ->
            Node(Helper left None, Helper right None)
            |> NoneDestroyer
        | Node (left, right), Node (left2, right2) ->
            Node(Helper left left2, Helper right right2)
            |> NoneDestroyer
        | _, _ -> failwith $"Something going wrong"

    if vec1.Length <> vec2.Length then
        failwith $"Different values of first vector length and second vector length"
    else
        SparseVector(Helper vec1.Memory vec2.Memory, vec1.Length)
