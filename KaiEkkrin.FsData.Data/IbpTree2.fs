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
    // - X < K1 for all keys X in P1
    // - Kn-1 <= X < Kn for all keys X in Pn
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
    // we won't include that in the structure but instead pass it to functions that need it.
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
    // Key(X) < K1 for all X in N1
    // K1 <= Key(X) for all X in N2
    type NodeUpdate<'TKey, 'TValue when 'TKey :> IComparable<'TKey> > =
        | Single of Node<'TKey, 'TValue>
        | Split of Node<'TKey, 'TValue> * 'TKey * Node<'TKey, 'TValue>

    type Tree<'TKey, 'TValue when 'TKey :> IComparable<'TKey> >(
        B: int, Comparer: IComparer<'TKey>, Root: Node<'TKey, 'TValue>
    ) =
        // ## Helpers ##

        let divCeil (a: int) b =
            let (div, rem) = Math.DivRem(a, b)
            if rem = 0 then div else div + 1

        let lengthOfSplitIntNode = (divCeil B 2) - 1
        let lengthOfSplitLeafNode = divCeil (B - 1) 2

        let rec findIndexInLeaf i key (node: LeafNode<'TKey, 'TValue>) =
            // TODO I could optimise this by assuming the array is in order (always true)
            // and using a binary chop
            if i = node.Values.Length then (i, false)
            else
                match Comparer.Compare(key, node.Values[i].Key) with
                | 0 -> (i, true)
                | n when n < 0 -> (i, false)
                | _ -> findIndexInLeaf (i + 1) key node

        let rec findIndexInInt i key (node: IntNode<'TKey, 'TValue>) =
            // TODO I could optimise this by assuming the array is in order (always true)
            // and using a binary chop
            if i = node.Nodes.Length then node.Nodes.Length
            else
                match Comparer.Compare(key, node.Nodes[i].Key) with
                | n when n < 0 -> i
                | _ -> findIndexInInt (i + 1) key node

        // ## Search ##

        let rec findInLeaf key (node: LeafNode<'TKey, 'TValue>) =
            match findIndexInLeaf 0 key node with
            | (index, true) -> Some node.Values[index]
            | _ -> None

        and findInInt key (node: IntNode<'TKey, 'TValue>) =
            let index = findIndexInInt 0 key node
            let searchNode =
                if index = node.Nodes.Length then node.Last else node.Nodes[index].Value

            findInNode key searchNode

        and findInNode key (node: Node<'TKey, 'TValue>) =
            match node with
            | Int intNode -> findInInt key intNode
            | Leaf leafNode -> findInLeaf key leafNode

        // ## Insert ##

        let rec insertInLeaf key value (node: LeafNode<'TKey, 'TValue>) =
            let (index, isExactMatch) = findIndexInLeaf 0 key node
            if isExactMatch then
                let newValues =
                    Array.init node.Values.Length (fun i ->
                        if i = index then KeyValuePair(key, value) else node.Values[i])

                newValues |> LeafNode |> Leaf |> Single

            elif node.Values.Length < B - 1 then
                // Don't need to split this
                let newLength = node.Values.Length + 1
                let newArray =
                    Array.init newLength (fun i ->
                        if i < index then node.Values[i]
                        elif i = index then KeyValuePair(key, value)
                        else node.Values[i - 1])

                newArray |> LeafNode |> Leaf |> Single

            else
                // Need to split this.
                // TODO : Try to make the split side that gets the new node be the
                // one that receives the fewest existing nodes (if there's a difference)
                // The online examples don't illustrate that but it feels like in the
                // case B=3 it's important, to avoid inserting into a 2 and getting a 3...
                let newArray1 =
                    Array.init lengthOfSplitLeafNode (fun i ->
                        if i < index then node.Values[i]
                        elif i = index then KeyValuePair(key, value)
                        else node.Values[i - 1])
                
                let newArray2 =
                    Array.init (node.Values.Length - lengthOfSplitLeafNode) (fun j ->
                        let i = j + lengthOfSplitLeafNode
                        if i < index then node.Values[i]
                        elif i = index then KeyValuePair(key, value)
                        else node.Values[i - 1])

                Split (newArray1 |> LeafNode |> Leaf, newArray2[0].Key, newArray2 |> LeafNode |> Leaf)

        and insertInInt key value (node: IntNode<'TKey, 'TValue>) =
            let index = findIndexInInt 0 key node
            let updated =
                if index = node.Nodes.Length then insertInNode key value node.Last
                else insertInNode key value node.Nodes[index].Value

            match updated with
            | Single update ->
                if index = node.Nodes.Length then
                    IntNode (node.Nodes, update) |> Int |> Single
                else
                    let newNodes =
                        Array.init node.Nodes.Length (fun i ->
                            if i = index then KeyValuePair(node.Nodes[i].Key, update)
                            else node.Nodes[i])

                    IntNode (newNodes, node.Last) |> Int |> Single

            | Split (head, tailKey, tail) ->
                if node.Nodes.Length < B - 1 then
                    // Don't need to split this
                    if index = node.Nodes.Length then
                        // I'm replacing node.Last
                        let newNodes =
                            Array.init (node.Nodes.Length + 1) (fun i ->
                                if i = index then KeyValuePair(tailKey, head)
                                else node.Nodes[i])

                        IntNode (newNodes, tail) |> Int |> Single

                    else
                        // I'm not replacing node.Last
                        let newNodes =
                            Array.init (node.Nodes.Length + 1) (fun i ->
                                if i < index then node.Nodes[i]
                                elif i = index then KeyValuePair(tailKey, head)
                                elif i = index + 1 then KeyValuePair(node.Nodes[i - 1].Key, tail)
                                else node.Nodes[i - 1])

                        IntNode (newNodes, node.Last) |> Int |> Single

                else
                    // TODO Do need to split this
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
