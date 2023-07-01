namespace KaiEkkrin.FsData.Data

open System
open System.Collections.Generic
open System.Text

// With reference to https://www.geeksforgeeks.org/introduction-of-b-tree/
module IbpTree =

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
    // TODO track the count
    type Tree<'TKey, 'TValue when 'TKey :> IComparable<'TKey> >(
        B: int, Comparer: IComparer<'TKey>, Root: IntNode<'TKey, 'TValue>) =

        // ## Debug print ##
        let rec debugPrintIntNode prefix (sb: StringBuilder) (node: IntNode<'TKey, 'TValue>) =
            for kv in node.Nodes do
                sb.AppendLine $"{prefix} {kv.Key} ->" |> ignore
                debugPrintNode $"{prefix}  " sb kv.Value

        and debugPrintLeafNode prefix (sb: StringBuilder) (node: LeafNode<'TKey, 'TValue>) =
            for kv in node.Values do
                sb.AppendLine $"{prefix} {kv.Key} = {kv.Value}" |> ignore

        and debugPrintNode prefix (sb: StringBuilder) (node: Node<'TKey, 'TValue>) =
            match node with
            | Int intNode -> debugPrintIntNode prefix sb intNode
            | Leaf leafNode -> debugPrintLeafNode prefix sb leafNode

        // ## Search for one item ##
        let rec findInIntNode key (node: IntNode<'TKey, 'TValue>) =
            let rec find (l: KeyValuePair<'TKey, Node<'TKey, 'TValue> > list) =
                match l with
                | [] -> None
                | x::_ when Comparer.Compare (key, x.Key) < 0 -> None
                | x::[] -> findInNode key x.Value
                | x::y::xs ->
                    if Comparer.Compare (key, y.Key) < 0
                    then findInNode key x.Value
                    else find (y::xs)

            find node.Nodes

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
                | x::xs when Comparer.Compare (key, x.Key) < 0 ->
                    (KeyValuePair (key, insertInNode key value x.Value))::xs
                | x::[] -> [KeyValuePair (x.Key, insertInNode key value x.Value)]
                | x::y::xs ->
                    if Comparer.Compare (key, y.Key) < 0
                    then (KeyValuePair (x.Key, insertInNode key value x.Value))::y::xs
                    else insert (y::xs)

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

        member this.RootNode = Root // for structured formatting

        member this.Insert key value =
            let newRoot = insertInIntNode key value Root
            Tree (B, Comparer, newRoot)

        member this.TryFind key = findInIntNode key Root

        override this.ToString () =
            // Debug print this thing
            let sb = StringBuilder ()
            sb.AppendFormat $"Tree({B}, " |> ignore
            debugPrintIntNode "" sb Root
            sb.Append ")" |> ignore
            sb.ToString ()

    let create<'TKey, 'TValue when 'TKey :> IComparable<'TKey> > cmp =
        new Tree<'TKey, 'TValue>(64_000 / sizeof<'TValue>, cmp, IntNode [])

    let createB<'TKey, 'TValue when 'TKey :> IComparable<'TKey> > (b, cmp) =
        new Tree<'TKey, 'TValue>(b, cmp, IntNode [])

    let empty<'TKey, 'TValue when 'TKey :> IComparable<'TKey> > = new Tree<'TKey, 'TValue>(64_000 / sizeof<'TValue>, Comparer<'TKey>.Default, IntNode [])

    let emptyB<'TKey, 'TValue when 'TKey :> IComparable<'TKey> > b = new Tree<'TKey, 'TValue>(b, Comparer<'TKey>.Default, IntNode [])

    let insert<'TKey, 'TValue when 'TKey :> IComparable<'TKey> > key value (tree: Tree<'TKey, 'TValue>) = tree.Insert key value

    let tryFind<'TKey, 'TValue when 'TKey :> IComparable<'TKey> > key (tree: Tree<'TKey, 'TValue>) = tree.TryFind key

