﻿module SparseVector

open System
open Microsoft.FSharp.Core

type Vector<'value> =
    struct
        val Memory: array<'value option>
        val Head: int
        val Length: int

        new(memory, head, length) =
            { Memory = memory
              Head = head
              Length = length }
    end

type BinaryTree<'value> =
    | Node of BinaryTree<'value> * BinaryTree<'value>
    | Leaf of 'value
    | None

let ClosestDegreeOf2 columns lines =
    int (
        2.0
        ** Math.Ceiling(Math.Log(float (max columns lines), 2.0))
    )

let Separator (vec: Vector<'value>) =
    Vector(vec.Memory, vec.Head, vec.Length / 2), Vector(vec.Memory, vec.Head + vec.Length / 2, vec.Length / 2)

let NoneDestroyer (tree: BinaryTree<'value>) =
    match tree with
    | Leaf value -> Leaf value
    | Node (None, None) -> None
    | _ -> tree

let Transformer (arr: array<'value option>) =
    let virtualLength = ClosestDegreeOf2 arr.Length 0
    let virtualVec = Vector(arr, 0, virtualLength)

    let rec helper (virtualVec: Vector<'value>) =
        if virtualVec.Head >= arr.Length then
            None
        elif virtualVec.Length = 1 then
            if virtualVec.Memory[virtualVec.Head] = Option.None then
                None
            else
                Leaf (virtualVec.Memory[virtualVec.Head]).Value
        else
            let leftTree = helper (fst (Separator virtualVec))
            let rightTree = helper (snd (Separator virtualVec))
            Node(leftTree, rightTree) |> NoneDestroyer

    if arr.Length = 0 then
        None
    else
        helper virtualVec

type SparseVector<'value when 'value: equality> =
    val Memory: BinaryTree<'value>
    val Length: int

    new(arr) =
        { Memory = Transformer arr
          Length = arr.Length }

    new(tree, length) = { Memory = tree; Length = length }

    member this.Item
        with get i =
            if i >= this.Length then
                failwith $"Index %A{i} is out of range."
            else
                let virtualLength = ClosestDegreeOf2 this.Length 0

                let rec GetElementByIndex tree length index =
                    match tree with
                    | None -> Option.None
                    | Leaf value -> Some value
                    | Node (left, right) ->
                        if index < length / 2 then
                            GetElementByIndex left (length / 2) index
                        else
                            GetElementByIndex right (length / 2) (index - length / 2)

                GetElementByIndex this.Memory virtualLength i

let GetValue x =
    match x with
    | Some value -> value
    | _ -> failwith $"No value"

let NoneOrValue x =
    match x with
    | Option.None -> None
    | _ -> Leaf(x |> GetValue)

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