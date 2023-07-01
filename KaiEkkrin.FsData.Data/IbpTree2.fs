namespace KaiEkkrin.FsData.Data

open System
open System.Collections.Generic

// With reference to
// - https://en.wikipedia.org/wiki/B%2B_tree
// - https://www.geeksforgeeks.org/introduction-of-b-tree/
// - https://www.programiz.com/dsa/b-plus-tree
// This defines a B+ tree of order B:
// (What's the lowest valid value of B, anyway?)
// TODO try optimising the Array.init into Array.zeroCreate / Array.Copy
// Can interop with Array.Copy by calling with named arguments, as described here:
// See https://fsharpforfunandprofit.com/posts/completeness-seamless-dotnet-interop/
module IbpTree2 =

    // ## Node types ##

    // An internal node contains P1, K1, P2, K2, ..., Pc-1, Kc-1, Pc
    // where ceil(B/2) <= c <= B
    // or in the case of the root node, 2 <= c <= B.
    // P pointers, K key-values
    // such that
    // - X <= K1 for all keys X in P1
    // - Kn-1 < X <= Kn for all keys X in Pn
    // - Kc-1 < X for all keys X in Pc
    type IntNode<'TKey, 'TValue when 'TKey :> IComparable<'TKey> > = struct
        // List of (P1, Kn), (P2, K2), ..., (Pc-1, Kc-1) as above
        // Key = Kx, Value = Px of course.
        val Nodes: KeyValuePair<'TKey, Node<'TKey, 'TValue> > []

        // Pc as above
        val Last: Node<'TKey, 'TValue> // Pc above

        new (nodes, last) = { Nodes = nodes; Last = last }
        end

    // A leaf node contains K1, V1, K2, V2, ..., Kc-1, Vc-1
    // where ceil(B/2) <= c <= B
    // or, if it's the root node (the only node in the tree), 0 <= c <= B
    // E.g. when B=3, a leaf node may contain 1 or 2 values.
    // In addition, each leaf node logically carries an optional pointer to the next one --
    // we won't include that in the structure but instead pass it to functions that need it
    and LeafNode<'TKey, 'TValue when 'TKey :> IComparable<'TKey> > = struct
        val Values: KeyValuePair<'TKey, 'TValue> []
        new values = { Values = values }
        end

    // TODO these can be made [<Struct>] -- experiment
    // See https://www.bartoszsypytkowski.com/writing-high-performance-f-code/
    and Node<'TKey, 'TValue when 'TKey :> IComparable<'TKey> > =
        | Int of IntNode<'TKey, 'TValue>
        | Leaf of LeafNode<'TKey, 'TValue>

    // When doing an insert, I'll either return a single node (updated with the new value)
    // or a split of it, (N1, K1, N2) where K is such that
    // Key(X) <= K1 for all X in N1
    // K1 < Key(X) for all X in N2
    type Split<'TKey, 'TValue when 'TKey :> IComparable<'TKey> > =
        | Single of Node<'TKey, 'TValue>
        | Split of Node<'TKey, 'TValue> * 'TKey * Node<'TKey, 'TValue>

    type Tree<'TKey, 'TValue when 'TKey :> IComparable<'TKey> >(
        B: int, Comparer: IComparer<'TKey>, Root: Node<'TKey, 'TValue>
    ) =
        let ceilHalfB =
            let (div, rem) = Math.DivRem(B, 2)
            if rem = 0 then div else div + 1

        let rec findInLeaf key (node: LeafNode<'TKey, 'TValue>) =
            // TODO I can binary search this, but the built-in functions only
            // provide exact matching :(
            Array.tryFind (fun (kv: KeyValuePair<'TKey, 'TValue>) ->
                Comparer.Compare(key, kv.Key) = 0) node.Values

        and findInInt key (node: IntNode<'TKey, 'TValue>) =
            let rec find index =
                if node.Nodes.Length = index then node.Last
                elif Comparer.Compare(key, node.Nodes[index].Key) <= 0 then node.Nodes[index].Value
                else find (index + 1)
            
            findInNode key (find 0)

        and findInNode key (node: Node<'TKey, 'TValue>) =
            match node with
            | Int intNode -> findInInt key intNode
            | Leaf leafNode -> findInLeaf key leafNode

        let rec insertInLeaf key value (node: LeafNode<'TKey, 'TValue>) =
            // TODO I can binary search this, do the optimisation?
            let rec findPosition index =
                if index >= node.Values.Length then (node.Values.Length, false)
                else
                    match Comparer.Compare(key, node.Values[index].Key) with
                    | 0 -> (index, true)
                    | n when n < 0 -> (index, false)
                    | _ -> findPosition (index + 1)

            let (index, isExactMatch) = findPosition 0
            if isExactMatch then
                let newValues = node.Values.Clone() :?> KeyValuePair<'TKey, 'TValue> []
                newValues[index] <- KeyValuePair(key, value)
                newValues |> LeafNode |> Leaf |> Single

            elif node.Values.Length < B - 1 then
                // Don't need to split this
                let newLength = node.Values.Length + 1
                let newArray =
                    if index = 0 then
                        Array.init newLength (fun i ->
                            if i = 0 then KeyValuePair(key, value)
                            else node.Values[i - 1])
                    elif index = node.Values.Length then
                        Array.init newLength (fun i ->
                            if i = index then KeyValuePair(key, value)
                            else node.Values[i])
                    else
                        Array.init newLength (fun i ->
                            if i < index then node.Values[i]
                            elif i = index then KeyValuePair(key, value)
                            else node.Values[i - 1])

                newArray |> LeafNode |> Leaf |> Single

            else
                // Need to split this
                failwith "TODO implement"

        and insertInInt key value (node: IntNode<'TKey, 'TValue>) =
            // TODO tail recursion
            let rec insert (nodes: KeyValuePair<'TKey, Node<'TKey, 'TValue> > list) last =
                match nodes with
                | [] -> [KeyValuePair(key, insertInNode key value last)]
                | x::xs when Comparer.Compare(key, x.Key) <= 0 ->
                    KeyValuePair(x.Key, insertInNode key value x.Value)::xs
                | x::xs -> x::insert xs last

            failwith "TODO Implement"

        and insertInNode key value (node: Node<'TKey, 'TValue>) =
            match node with
            | Int intNode -> insertInInt key value intNode
            | Leaf leafNode -> insertInLeaf key value leafNode

    let bValueFor<'T> = Math.Max (3, 64_000 / sizeof<'T>)

    let create<'TKey, 'TValue when 'TKey :> IComparable<'TKey> > cmp =
        new Tree<'TKey, 'TValue>(bValueFor<'TKey>, cmp, LeafNode [||] |> Leaf)

    let createB<'TKey, 'TValue when 'TKey :> IComparable<'TKey> > (b, cmp) =
        if b < 3 then raise <| ArgumentException("b must be at least 3", nameof(b))
        new Tree<'TKey, 'TValue>(b, cmp, LeafNode [||] |> Leaf)

    let empty<'TKey, 'TValue when 'TKey :> IComparable<'TKey> > =
        new Tree<'TKey, 'TValue>(bValueFor<'TKey>, Comparer<'TKey>.Default, LeafNode [||] |> Leaf)

    let emptyB<'TKey, 'TValue when 'TKey :> IComparable<'TKey> > b =
        if b < 3 then raise <| ArgumentException("b must be at least 3", nameof(b))
        new Tree<'TKey, 'TValue>(b, Comparer<'TKey>.Default, LeafNode [||] |> Leaf)
