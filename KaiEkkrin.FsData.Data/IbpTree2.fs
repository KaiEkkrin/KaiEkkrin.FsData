namespace KaiEkkrin.FsData.Data

open System
open System.Collections.Generic
open System.Text

// With reference to
// - https://en.wikipedia.org/wiki/B%2B_tree
// - https://www.geeksforgeeks.org/introduction-of-b-tree/
// - https://www.programiz.com/dsa/b-plus-tree
// This defines a B+ tree of order B:
// (What's the lowest valid value of B, anyway?)
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
    type IntNode<'TKey, 'TValue> = struct
        // List of (P1, Kn), (P2, K2), ..., (Pc-1, Kc-1) as above
        // Key = Kx, Value = Px of course.
        val Nodes: KeyValuePair<'TKey, Node<'TKey, 'TValue> > []

        // Pc as above
        val Last: Node<'TKey, 'TValue> // Pc above

        new (nodes, last) = { Nodes = nodes; Last = last }
        end
    with
        override this.ToString() =
            this.Nodes |> Array.map (fun kv -> kv.Key) |> sprintf "%A"

    // A leaf node contains K1, V1, K2, V2, ..., Kc-1, Vc-1
    // where ceil(B/2) <= c <= B
    // or, if it's the root node (the only node in the tree), 0 <= c <= B
    // E.g. when B=3, a leaf node may contain 1 or 2 values.
    // In addition, each leaf node logically carries an optional pointer to the next one --
    // we won't include that in the structure but instead pass it to functions that need it.
    and LeafNode<'TKey, 'TValue> = struct
        val Values: KeyValuePair<'TKey, 'TValue> []
        new values = { Values = values }
        end
    with
        override this.ToString() =
            this.Values |> Array.map (fun kv -> sprintf "%A = %A" kv.Key kv.Value) |> sprintf "%A"

    // TODO these can be made [<Struct>] -- experiment?
    // See https://www.bartoszsypytkowski.com/writing-high-performance-f-code/
    and Node<'TKey, 'TValue> =
        | Int of IntNode<'TKey, 'TValue>
        | Leaf of LeafNode<'TKey, 'TValue>

    // When doing an insert, I'll either return a single node (updated with the new value)
    // or a split of it, (N1, K1, N2) where K is such that
    // Key(X) < K1 for all X in N1
    // K1 <= Key(X) for all X in N2
    type NodeUpdate<'TKey, 'TValue> =
        | Single of Node<'TKey, 'TValue>
        | Split of Node<'TKey, 'TValue> * 'TKey * Node<'TKey, 'TValue>

    // The result of validating a node.
    type NodeValidationResult<'TKey, 'TValue> =
        | NotValid of string
        | Valid of 'TKey []

    let childrenOfInt (node: IntNode<'TKey, 'TValue>) = seq {
        for kv in node.Nodes do
            yield kv.Value

        yield node.Last
    }

    type Tree<'TKey, 'TValue>(
        B: int, Comparer: IComparer<'TKey>, Root: Node<'TKey, 'TValue>
    ) =
        // ## Helpers ##

        let divCeil (a: int) b =
            let (div, rem) = Math.DivRem(a, b)
            if rem = 0 then div else div + 1

        let lengthOfSplitIntNode = (divCeil B 2) - 1
        let lengthOfSplitLeafNode = divCeil (B - 1) 2

        let findIndexInLeaf key (node: LeafNode<'TKey, 'TValue>) =
            // The node is sorted in ascending order of keys.
            // I'm looking for either the index where the node's key equals the given one
            // or the index of the first node with a key less than the given one.
            // TODO I could optimise this by assuming the array is in order (always true)
            // and using a binary chop
            let rec doFind i =
                if i = node.Values.Length then (i, false)
                else
                    match Comparer.Compare(key, node.Values[i].Key) with
                    | 0 -> (i, true)
                    | n when n < 0 -> (i, false)
                    | _ -> doFind (i + 1)

            doFind 0

        let findIndexInInt key (node: IntNode<'TKey, 'TValue>) =
            // The node is sorted in ascending order of keys.
            // I'm looking for the index of the first node such that the given key
            // is less than the node's key.
            // TODO I could optimise this by assuming the array is in order (always true)
            // and using a binary chop
            let rec doFind i =
                if i = node.Nodes.Length then node.Nodes.Length // it goes into `node.Last`
                elif Comparer.Compare(key, node.Nodes[i].Key) < 0 then i
                else doFind (i + 1)

            doFind 0

        // ## Debug: check widths ##

        let rec debugCheckWidthsNode maybeMinWidth node =
            match node with
            | Int intNode -> debugCheckWidthsInt maybeMinWidth intNode
            | Leaf leafNode -> debugCheckWidthsLeaf maybeMinWidth leafNode

        and debugCheckWidthsInt maybeMinWidth node =
            let minWidth = match maybeMinWidth with | Some a -> a | None -> lengthOfSplitIntNode
            if node.Nodes.Length < minWidth then Some <| sprintf "Node %A less than width %d" node minWidth
            elif node.Nodes.Length >= B then Some <| sprintf "Node %A ge than width %d" node B
            else
                childrenOfInt node
                |> Seq.fold (fun x n ->
                    match x with
                    | Some err -> Some err
                    | None -> debugCheckWidthsNode None n) None

        and debugCheckWidthsLeaf maybeMinWidth node =
            let minWidth = match maybeMinWidth with | Some a -> a | None -> lengthOfSplitLeafNode
            if node.Values.Length < minWidth then Some <| sprintf "Node %A less than width %d" node minWidth
            elif node.Values.Length >= B then Some <| sprintf "Node %A ge than width %d" node B
            else None

        // ## Debug: count leaf nodes ##

        let rec debugCountLeafNodes node =
            match node with
            | Int intNode -> debugCountLeafNodesInt intNode
            | Leaf _ -> 1

        and debugCountLeafNodesInt node = childrenOfInt node |> Seq.sumBy debugCountLeafNodes

        // ## Debug: get depth ##

        let rec debugGetDepthNode node =
            match node with
            | Int intNode -> debugGetDepthInt intNode
            | Leaf _ -> 1

        and debugGetDepthInt node =
            let depths =
                childrenOfInt node
                |> Seq.map debugGetDepthNode
                |> Set.ofSeq

            if depths.Count > 1 then failwithf "At node %A, found differing depths: %A" node depths
            depths |> Seq.head

        // ## Debug: formatted print ##

        let rec debugPrintNode prefix (sb: StringBuilder) node =
            match node with
            | Int intNode -> debugPrintInt prefix sb intNode
            | Leaf leafNode -> debugPrintLeaf prefix sb leafNode

        and debugPrintInt prefix sb node =
            let keyStrings = node.Nodes |> Array.map (fun kv -> $"{kv.Key}")
            let keyStringMaxLength = keyStrings |> Seq.map (fun s -> s.Length) |> Seq.max
            let prefixExtension = Array.create keyStringMaxLength ' ' |> fun cs -> new String(cs)
            for kv in node.Nodes do
                debugPrintNode $"{prefix}...{prefixExtension}" sb kv.Value
                sb.AppendLine $"{prefix} < {kv.Key}" |> ignore

            debugPrintNode $"{prefix}...{prefixExtension}" sb node.Last

        and debugPrintLeaf prefix sb node =
            for kv in node.Values do
                sb.AppendLine $"{prefix} {kv.Key} = {kv.Value}" |> ignore
    
        // ## Debug: validate tree ##

        let rec debugValidateNode node =
            match node with
            | Int intNode -> debugValidateInt intNode
            | Leaf leafNode -> debugValidateLeaf leafNode

        and debugValidateInt node =
            let keys = node.Nodes |> Array.map (fun kv -> kv.Key)
            let keySet = SortedSet<'TKey>(Comparer)

            // Keys must be unique and ordered
            // TODO : *Sub-keys* must be unique and ordered too -- check! That's currently falling
            // through the net
            let isValid =
                keys
                |> Seq.zip keySet
                |> Seq.fold (fun acc (a, b) ->
                    match acc with
                    | None -> if Comparer.Compare(a, b) = 0 then None else Some <| sprintf "Int node invalid keys: %A" node
                    | Some err -> Some err
                ) None

            match isValid with
            | Some err -> NotValid err
            | None ->
                // Each of those should validate successfully
                node.Nodes
                |> Seq.mapi (fun i kv ->
                    (if i = 0 then None else Some node.Nodes[i - 1].Key), Some kv.Key, kv.Value)
                |> Seq.append [|(Some node.Nodes[node.Nodes.Length - 1].Key, None, node.Last)|]
                |> Seq.fold (fun result (lb, ub, n) ->
                    match (result, debugValidateNode n, lb, ub) with
                    | NotValid err, _, _, _ -> NotValid err
                    | _, NotValid err, _, _ -> NotValid err
                    | _, Valid subKeys, None, Some upperBound ->
                        // Each of the subKeys must be < upperBound
                        subKeys
                        |> Array.fold (fun v k ->
                            match v with
                            | NotValid err -> NotValid err
                            | Valid w ->
                                if Comparer.Compare(k, upperBound) < 0 then Valid w
                                else NotValid <| sprintf "In int node %A: Requires sub-key %A < %A" node k upperBound
                        ) (Valid keys)

                    | _, Valid subKeys, Some lowerBound, Some upperBound ->
                        // Each of the subKeys must be: lowerBound <= subKey < upperBound
                        subKeys
                        |> Array.fold (fun v k ->
                            match v with
                            | NotValid err -> NotValid err
                            | Valid w ->
                                if Comparer.Compare(lowerBound, k) <= 0 && Comparer.Compare(k, upperBound) < 0 then Valid w
                                else NotValid <| sprintf "In int node %A: Requires %A <= sub-key %A < %A" node lowerBound k upperBound
                        ) (Valid keys)

                    | _, Valid subKeys, Some lowerBound, None ->
                        // Each of the subKeys must be: lowerBound <= subKey
                        subKeys
                        |> Array.fold (fun v k ->
                            match v with
                            | NotValid err -> NotValid err
                            | Valid w ->
                                if Comparer.Compare(lowerBound, k) <= 0 then Valid w
                                else NotValid <| sprintf "In int node %A: Requires %A <= sub-key %A" node lowerBound k
                        ) (Valid keys)

                    | _, _, None, None -> failwithf "In int node %A: Error -- No key bounds" node

                ) (Valid keys)

        and debugValidateLeaf node =
            let keys = node.Values |> Array.map (fun kv -> kv.Key)
            let keySet = SortedSet<'TKey>(Comparer)

            // Keys must be unique and ordered
            let isValid =
                keys
                |> Seq.zip keySet
                |> Seq.fold (fun ok (a, b) -> ok && Comparer.Compare(a, b) = 0) true

            if isValid then Valid keys else NotValid <| sprintf "Invalid leaf: %A" node

        // ## Debug: other things ##

        let debugKeysShallow node =
            match node with
            | Int intNode -> intNode.Nodes |> Array.map (fun kv -> kv.Key)
            | Leaf leafNode -> leafNode.Values |> Array.map (fun kv -> kv.Key)

        // ## Enumerate all key-value pairs in order ##

        let rec enumerateAll node =
            match node with
            | Int intNode -> enumerateAllInt intNode
            | Leaf leafNode -> leafNode.Values |> Seq.ofArray

        and enumerateAllInt node = childrenOfInt node |> Seq.collect enumerateAll

        // ## Search ##

        let rec findInNode key node =
            match node with
            | Int intNode -> findInInt key intNode
            | Leaf leafNode -> findInLeaf key leafNode

        and findInInt key node =
            let index = findIndexInInt key node
            let searchNode =
                if index = node.Nodes.Length then node.Last else node.Nodes[index].Value

            findInNode key searchNode

        and findInLeaf key node =
            match findIndexInLeaf key node with
            | (index, true) -> Some node.Values[index].Value
            | _ -> None

        // ## Search (forward sequence) ##

        let rec findSeqInNode key node =
            match node with
            | Int intNode -> findSeqInInt key intNode
            | Leaf leafNode -> findSeqInLeaf key leafNode

        and findSeqInInt key node =
            let index = findIndexInInt key node
            if index = node.Nodes.Length then
                // Only need to search the last node
                findSeqInNode key node.Last
            else
                // Need to search from `index`, including the last
                Seq.append (node.Nodes[index..] |> Seq.map (fun n -> n.Value)) [|node.Last|]
                |> Seq.collect (findSeqInNode key)

        and findSeqInLeaf key node =
            match findIndexInLeaf key node with
            | (index, _) when index < node.Values.Length ->
                node.Values[index..] |> Seq.ofArray
            | _ -> Seq.empty

        // ## Insert ##

        let rec insertInNode key value node =
            match node with
            | Int intNode -> insertInInt key value intNode
            | Leaf leafNode -> insertInLeaf key value leafNode

        and insertInLeaf key value node =
            let (index, isExactMatch) = findIndexInLeaf key node
            if isExactMatch then
                // This is a straightforward value replacement and never needs splitting
                let newValues =
                    ArrayUtil.arraySplice1 index 1 (KeyValuePair(key, value)) node.Values

                newValues |> LeafNode |> Leaf |> Single

            else
                // Sanity check
                if node.Values.Length >= B then
                    failwithf "Insert %A: found leaf node with %d values (B=%d)" key node.Values.Length B

                // Create the combined array
                let newValues =
                    ArrayUtil.arraySplice1 index 0 (KeyValuePair(key, value)) node.Values

                if newValues.Length < B then
                    newValues |> LeafNode |> Leaf |> Single

                else
                    // This is too big, split it
                    // Note an F# array slice is *inclusive of the last index*
                    let newValues1 = newValues[..(lengthOfSplitLeafNode - 1)]
                    let newValues2 = newValues[lengthOfSplitLeafNode..]

                    // TODO remove debug, checking this can't happen
                    //if Comparer.Compare(newValues1[lengthOfSplitLeafNode - 1].Key, newValues2[0].Key) >= 0 then
                    //    failwithf "Erroneously split leaf with ...%A, %A..." newValues1[lengthOfSplitLeafNode - 1].Key newValues2[0].Key
                
                    Split (newValues1 |> LeafNode |> Leaf, newValues2[0].Key, newValues2 |> LeafNode |> Leaf)

        and insertInInt key value node =
            // Do the recursive insert
            let index = findIndexInInt key node
            let update =
                if index = node.Nodes.Length then insertInNode key value node.Last
                else insertInNode key value node.Nodes[index].Value

            match update with
            | Single u ->
                // This can be inserted without any resize of the current node and no change in key
                if index = node.Nodes.Length then
                    IntNode (node.Nodes, u) |> Int |> Single
                else
                    let newItem = KeyValuePair(node.Nodes[index].Key, u)
                    let newNodes =
                        ArrayUtil.arraySplice1 index 1 newItem node.Nodes

                    IntNode (newNodes, node.Last) |> Int |> Single

            | Split (head, tailKey, tail) ->
                // Sanity check
                if node.Nodes.Length >= B then
                    failwithf "Insert %A: found internal node with %d keys (B=%d)" key node.Nodes.Length B

                // Create the updated node (which may be overly large)
                let updated =
                    if index = node.Nodes.Length then
                        // Add this onto the end
                        let newItem = KeyValuePair(tailKey, head)
                        let newNodes =
                            ArrayUtil.arraySplice1 index 0 newItem node.Nodes
                            
                        IntNode (newNodes, tail)
                    else
                        // Add this within the array.
                        // - The matched index gets the head pointer and the new tail key (indicating the
                        // smallest key found in the tail)
                        // - The index after that gets the tail pointer and the key that used to come after
                        // the matched index (indicating the smallest key found in the node after the
                        // matched one)
                        let newItem1 = KeyValuePair(tailKey, head)
                        let newItem2 = KeyValuePair(node.Nodes[index].Key, tail)
                        let newNodes =
                            ArrayUtil.arraySpliceX index 1 [|newItem1; newItem2|] node.Nodes

                        IntNode (newNodes, node.Last)

                // TODO remove debug. Validate the updated node.
                //let newKeyArray = updated.Nodes |> Array.map (fun kv -> kv.Key)
                //let newKeySet = SortedSet<'TKey>(Comparer)
                //newKeyArray |> Array.iter (fun e -> newKeySet.Add e |> ignore)
                //if newKeySet.Count <> newKeyArray.Length then
                //    let sb = StringBuilder ()
                //    sprintf "B=%d. On update key = %A (index = %d):" B key index |> sb.AppendLine |> ignore
                //    sb.AppendLine "Original:" |> ignore
                //    debugPrintInt "  " sb node
                //    sb.AppendLine "Updated:" |> ignore
                //    debugPrintInt "  " sb updated
                //    sprintf "Bad updated node: new keys = %A" newKeyArray |> sb.AppendLine |> ignore
                //    failwith <| sb.ToString()

                if updated.Nodes.Length < B then
                    // No further splitting is required
                    updated |> Int |> Single

                else
                    // This is too big, split it.
                    // The node at index lengthOfSplitIntNode becomes the left-hand side's last node, and
                    // the key at that index becomes the `tailKey` returned in the split update.
                    // Again, note that an F# array slice is *inclusive of the last index*
                    let newNodes1 = updated.Nodes[..(lengthOfSplitIntNode - 1)]
                    let newNodes2 = updated.Nodes[(lengthOfSplitIntNode + 1)..]
                    let head = IntNode (newNodes1, updated.Nodes[lengthOfSplitIntNode].Value)
                    let tailKey = updated.Nodes[lengthOfSplitIntNode].Key
                    let tail = IntNode (newNodes2, updated.Last)

                    // TODO remove debug. Validate this split.
                    //let keysInTail = tail.Nodes |> Array.map (fun kv -> kv.Key)
                    //keysInTail |> Array.iter (fun k ->
                    //    if Comparer.Compare(k, tailKey) <= 0 then
                    //        failwithf "B=%d, split at %d: Bad split of %A into head = %A, tailKey = %A, tail = %A" B lengthOfSplitIntNode updated head tailKey tail
                    //)

                    Split (head |> Int, tailKey, tail |> Int)

        // ## Public methods ##

        member this.DebugValidate () =
            // Count tree and check depth.
            // See https://cs.stackexchange.com/questions/82015/maximum-depth-of-a-b-tree
            let depth = debugGetDepthNode Root
            let leafCount = debugCountLeafNodes Root
            let maxDepth = 1 + (Math.Log (leafCount, (float B) / 2.0) |> Math.Ceiling |> int)
            if depth > maxDepth then Some <| sprintf "Required max depth = %d, found %d" maxDepth depth
            else
                // Check other things
                match (debugCheckWidthsNode (Some 0) Root, debugValidateNode Root) with
                | (Some err, _) -> Some err
                | (_, NotValid err) -> Some err
                | _ -> None

        member this.EnumerateFrom key = findSeqInNode key Root

        member this.Insert key value =
            match insertInNode key value Root with
            | Single u ->
                // This forms the updated root node
                Tree(B, Comparer, u)

            | Split (head, tailKey, tail) ->
                // Generate a new root node out of this split:
                let newRoot = IntNode ([|KeyValuePair(tailKey, head)|], tail) |> Int
                Tree(B, Comparer, newRoot)

        member this.TryFind key = findInNode key Root

        override this.ToString () =
            // Debug print
            let sb = StringBuilder ()
            sb.AppendLine $"Tree({B}, " |> ignore
            debugPrintNode "" sb Root
            sb.AppendLine ")" |> ignore
            sb.ToString ()

        interface System.Collections.IEnumerable with
            member this.GetEnumerator () =
                (enumerateAll Root).GetEnumerator ()

        interface IEnumerable<KeyValuePair<'TKey, 'TValue> > with
            member this.GetEnumerator () =
                (enumerateAll Root).GetEnumerator ()

    let bValueFor<'T> = Math.Max (3, 64_000 / sizeof<'T>)

    let create<'TKey, 'TValue> cmp =
        new Tree<'TKey, 'TValue>(bValueFor<'TKey>, cmp, LeafNode [||] |> Leaf)

    let createB<'TKey, 'TValue> (b, cmp) =
        if b < 3 then raise <| ArgumentException("b must be at least 3", nameof(b))
        new Tree<'TKey, 'TValue>(b, cmp, LeafNode [||] |> Leaf)

    let empty<'TKey, 'TValue> =
        new Tree<'TKey, 'TValue>(bValueFor<'TKey>, Comparer<'TKey>.Default, LeafNode [||] |> Leaf)

    let emptyB<'TKey, 'TValue> b =
        if b < 3 then raise <| ArgumentException("b must be at least 3", nameof(b))
        new Tree<'TKey, 'TValue>(b, Comparer<'TKey>.Default, LeafNode [||] |> Leaf)

    let debugValidate<'TKey, 'TValue> (tree: Tree<'TKey, 'TValue>) =
        tree.DebugValidate ()

    let enumerateFrom<'TKey, 'TValue> key (tree: Tree<'TKey, 'TValue>) =
        tree.EnumerateFrom key

    let insert<'TKey, 'TValue> key value (tree: Tree<'TKey, 'TValue>) =
        tree.Insert key value

    let tryFind<'TKey, 'TValue> key (tree: Tree<'TKey, 'TValue>) =
        tree.TryFind key
