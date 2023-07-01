namespace KaiEkkrin.FsData.Data

open System
open System.Collections.Generic

// With reference to https://www.geeksforgeeks.org/introduction-of-b-tree/
module IpbTree =

    // Node types.
    type IntNode<'TKey, 'TValue when 'TKey :> IComparable<'TKey> > = struct
        val Nodes: KeyValuePair<'TKey, Node<'TKey, 'TValue> > list
        new nodes = { Nodes = nodes }
        end

    and LeafNode<'TKey, 'TValue when 'TKey :> IComparable<'TKey> > = struct
        val Values: KeyValuePair<'TKey, 'TValue> list
        new values = { Values = values }
        end

    and Node<'TKey, 'TValue when 'TKey :> IComparable<'TKey> > =
        | Int of IntNode<'TKey, 'TValue>
        | Leaf of LeafNode<'TKey, 'TValue>

    // Overall wrapper and settings for the tree.
    type Tree<'TKey, 'TValue when 'TKey :> IComparable<'TKey> >(
        B: int, Comparer: IComparer<'TKey>, Root: IntNode<'TKey, 'TValue>) =

        // ## Search for one item ##
        let rec findInIntNode key (node: IntNode<'TKey, 'TValue>) =
            match List.tryFind (fun (n: KeyValuePair<'TKey, Node<'TKey, 'TValue> >) -> Comparer.Compare (key, n.Key) = 0) node.Nodes with
            | Some kv -> findInNode key kv.Value
            | None -> None

        and findInLeafNode key (node: LeafNode<'TKey, 'TValue>) =
            List.tryFind (fun (v: KeyValuePair<'TKey, 'TValue>) -> Comparer.Compare (key, v.Key) = 0) node.Values

        and findInNode key node =
            match node with
            | Int intNode -> findInIntNode key intNode
            | Leaf leafNode -> findInLeafNode key leafNode

        // ## Insertion of one item ##
        let rec insertInIntNode key value (node: IntNode<'TKey, 'TValue>) =
            let rec insert (l: KeyValuePair<'TKey, Node<'TKey, 'TValue> > list) =
                match l with
                | [] -> [KeyValuePair (key, insertInLeafNode key value (LeafNode []) |> Leaf)]
                | x::[] -> [KeyValuePair (key, insertInNode key value x.Value)]
                | x::y::xs ->
                    match Comparer.Compare (key, y.Key) with
                    | n when n < 0 -> (KeyValuePair (key, insertInNode key value x.Value))::y::xs
                    | _ -> insert (y::xs)

            // TODO splitting as required.
            insert node.Nodes |> IntNode<'TKey, 'TValue>

        and insertInLeafNode key value (node: LeafNode<'TKey, 'TValue>) =
            let rec insert (l: KeyValuePair<'TKey, 'TValue> list) =
                match l with
                | [] -> [KeyValuePair (key, value)]
                | x::xs ->
                    match Comparer.Compare (key, x.Key) with
                    | 0 -> (KeyValuePair (key, value))::xs
                    | n when n < 0 -> (KeyValuePair (key, value))::x::xs
                    | _ -> x::(insert xs)

            // TODO splitting as required.
            insert node.Values |> LeafNode<'TKey, 'TValue>

        and insertInNode key value node =
            match node with
            | Int intNode -> insertInIntNode key value intNode |> Int
            | Leaf leafNode -> insertInLeafNode key value leafNode |> Leaf

        // ## Public methods ##

        member this.Insert key value =
            let newRoot = insertInIntNode key value Root
            Tree (B, Comparer, newRoot)

        member this.TryFind key = findInIntNode key Root

    let create<'TKey, 'TValue when 'TKey :> IComparable<'TKey> > cmp =
        new Tree<'TKey, 'TValue>(64_000 / sizeof<'TValue>, cmp, IntNode [])

    let createB<'TKey, 'TValue when 'TKey :> IComparable<'TKey> > (b, cmp) =
        new Tree<'TKey, 'TValue>(b, cmp, IntNode [])

    let insert<'TKey, 'TValue when 'TKey :> IComparable<'TKey> > key value (tree: Tree<'TKey, 'TValue>) = tree.Insert key value

    let tryFind<'TKey, 'TValue when 'TKey :> IComparable<'TKey> > key (tree: Tree<'TKey, 'TValue>) = tree.TryFind key

