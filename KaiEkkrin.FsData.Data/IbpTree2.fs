namespace KaiEkkrin.FsData.Data

open System
open System.Buffers
open System.Collections.Generic
open System.Text

// With reference to
// - https://en.wikipedia.org/wiki/B%2B_tree
// - https://www.geeksforgeeks.org/introduction-of-b-tree/
// - https://www.programiz.com/dsa/b-plus-tree
// This defines a B+ tree of order B:
// (What's the lowest valid value of B, anyway?)
// TODO : After the basic thing is working -- for better performance whilst retaining the flexibility
// of immutability, try making a similar Freezable tree (or extending this one to be Freezable.)
// This would:
// - support mutable updates (until Frozen)
// - after being Frozen, become immutable
// - an update to a Frozen tree would return a new tree, not Frozen, but sharing unchanged data with
// the Frozen one
// To avoid it needing lots of allocations anyway I think such a tree would need to always allocate
// maximum size nodes. But, in the usage model I envisage (a "main line" tree that needs to be immutable,
// with threads doing flurries of short-lived edits to it), this could be advantageous
// This is also analogous to ImmutableDictionary<,>.Builder and I could even consider calling it such.
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
        // TODO For better memory locality, should I make this an array of length c instead, with
        // the pointer currently in `Last` being in the final position in the array instead,
        // and the final key being unused?
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

    and Node<'TKey, 'TValue> =
        | Int of IntNode<'TKey, 'TValue>
        | Leaf of LeafNode<'TKey, 'TValue>

    let emptyLeaf<'TKey, 'TValue> = Leaf (LeafNode (Array.empty< KeyValuePair<'TKey, 'TValue> >))

    // When doing a bulk create, I'll return a list of nodes associated with their minimum key:
    // TODO try writing as an outref, and returning just the depth?
    type NodeCreation<'TKey, 'TValue> = struct
        val Depth: int
        val MinKey: 'TKey
        val Node: Node<'TKey, 'TValue>
        new (depth, minKey, node) = { Depth = depth; MinKey = minKey; Node = node }
        end

    // When doing an insert, I'll either return a single node (updated with the new value)
    // or a split of it, (N1, K1, N2) where K is such that
    // Key(X) < K1 for all X in N1
    // K1 <= Key(X) for all X in N2
    // For performance, these should be emitted from functions as out refs
    [<Struct>]
    type NodeInsertion<'TKey, 'TValue> =
        | Inserted of Inserted: Node<'TKey, 'TValue>
        | Updated of Updated: Node<'TKey, 'TValue>
        | Split of Split: struct (Node<'TKey, 'TValue> * 'TKey * Node<'TKey, 'TValue>)

    // The result of doing a delete.
    // For performance, these should be emitted from functions as out refs
    [<Struct>]
    type NodeDeletion<'TKey, 'TValue> =
        | NotPresent // key not found in leaf; nothing to delete
        | Kept of Kept: struct ('TKey * Node<'TKey, 'TValue>) // the node, updated but not merged, with its new min key
        | BorrowedLeft of BorrowedLeft: struct (Node<'TKey, 'TValue> * 'TKey * Node<'TKey, 'TValue>) // borrowed from the left -- left min key can't have changed
        | BorrowedRight of BorrowedRight: struct ('TKey * Node<'TKey, 'TValue> * 'TKey * Node<'TKey, 'TValue>) // borrowed from the right -- right min key may have changed
        | MergedLeft of MergedLeft: Node<'TKey, 'TValue> // merged to the left -- min key is as the left sibling's
        | MergedRight of MergedRight: struct ('TKey * Node<'TKey, 'TValue>) // merged to the right -- min key may have changed
        | Deleted // the node is gone completely

    // The result of validating a node.
    type NodeValidationResult<'TKey, 'TValue> =
        | NotValid of string
        | Valid of 'TKey []

    let childrenOfInt (node: IntNode<'TKey, 'TValue>) = seq {
        for kv in node.Nodes do
            yield kv.Value

        yield node.Last
    }

    let getLengthOfSplitIntNode b = (Common.divCeil b 2) - 1
    let getLengthOfSplitLeafNode b = Common.divCeil (b - 1) 2

    // ## Create from sorted array ##

    let creationsToIntNode<'TKey, 'TValue> (creations: NodeCreation<'TKey, 'TValue> []) =
        // Remember, in the IntNode `nodes` array, for each key-value pair kv, kv.Value is
        // the node containing items with key < kv.Key.
        let nodes =
            creations[..(creations.Length - 2)]
            |> Array.mapi (fun i x -> KeyValuePair(creations[i + 1].MinKey, x.Node))

        let intNode = IntNode (nodes, creations[creations.Length - 1].Node)
        NodeCreation (creations[0].Depth + 1, creations[0].MinKey, Int intNode)

    let createSubtree<'TKey, 'TValue> b (array: KeyValuePair<'TKey, 'TValue> []) =
        // Deal with the "none" situation separately
        if array.Length = 0 then [||]
        else
            // Create the leaf nodes
            let lengthOfSplitLeafNode = getLengthOfSplitLeafNode b
            let leafNodes =
                ArrayUtil.splitIntoChunks lengthOfSplitLeafNode (b - 1) array
                |> Array.map (fun c -> NodeCreation (1, c[0].Key, c |> LeafNode |> Leaf))

            // Remember, each NodeCreation corresponds to a child node in the internal
            // node, not to a key in it; there's always one more child nodes than keys
            let minIntChunkSize = (getLengthOfSplitIntNode b) + 1

            let rec createSubtreeRec (array: NodeCreation<'TKey, 'TValue> []) =
                if array.Length < b then array
                else
                    ArrayUtil.splitIntoChunks minIntChunkSize b array
                    |> Array.map creationsToIntNode
                    |> createSubtreeRec

            createSubtreeRec leafNodes

    // ## Enumeration stacks ##

    // Borrow arrays of NodeStackFrame from the array pool for better performance

    type NodeStackFrame<'TKey, 'TValue> = struct
        val mutable Index: int
        val mutable Node: Node<'TKey, 'TValue>
        new (index, node) = { Index = index; Node = node }
        end

    // ## The tree data type ##

    type Tree<'TKey, 'TValue>(
        B: int, Count: int, Depth: int, Comparer: IComparer<'TKey>, Root: Node<'TKey, 'TValue>
    ) =
        // ## Helpers ##

        let lengthOfSplitIntNode = getLengthOfSplitIntNode B
        let lengthOfSplitLeafNode = getLengthOfSplitLeafNode B

        let findIndexInLeaf key (node: LeafNode<'TKey, 'TValue>) (index: outref<int>) =
            // The node is sorted in ascending order of keys.
            // I'm looking for either the index where the node's key equals the given one
            // or the index of the first node with a key less than the given one.
            // Finds within a range of the array between a (inclusive) and b (exclusive.)

            let mutable a = 0
            let mutable b = node.Values.Length
            let mutable notFound = true
            let mutable exactMatch = false
            index <- b

            // Binary chop for large windows
            while notFound && (b - a) >= 8 do
                let i = (a + b) / 2
                match Comparer.Compare (key, node.Values[i].Key) with
                | 0 ->
                    index <- i
                    exactMatch <- true
                    notFound <- false

                | n when n < 0 -> b <- i
                | _ -> a <- i + 1

            // Iterate over smaller windows
            while notFound && a < b do
                match Comparer.Compare (key, node.Values[a].Key) with
                | 0 ->
                    index <- a
                    exactMatch <- true
                    notFound <- false

                | n when n < 0 ->
                    index <- a
                    notFound <- false

                | _ -> a <- a + 1

            if a = b then index <- b
            exactMatch

        let findIndexInInt key (node: IntNode<'TKey, 'TValue>) =
            // The node is sorted in ascending order of keys.
            // I'm looking for the index of the first node such that the given key
            // is less than the node's key.

            let mutable a = 0
            let mutable b = node.Nodes.Length
            let mutable notFound = true

            // Binary chop for large windows
            while notFound && (b - a) >= 16 do
                let i = (a + b) / 2
                match Comparer.Compare (key, node.Nodes[i].Key) with
                | 0 ->
                    a <- i + 1
                    notFound <- false

                | n when n < 0 -> b <- i
                | _ -> a <- i + 1

            while notFound && a < b do
                if Comparer.Compare (key, node.Nodes[a].Key) < 0 then notFound <- false
                else a <- a + 1

            if notFound then b else a

        // ## Enumerate all key-value pairs in order ##

        let enumerateAll node = seq {
            // TODO if this reduces allocations, clean up the interface and make the partial enumerate
            // be able to use it too, avoiding the seq { ... } wrapping.
            let stack = ArrayPool< NodeStackFrame<'TKey, 'TValue> >.Shared.Rent Depth
            stack[0].Index <- 0
            stack[0].Node <- node
            use enumerator = new TreeFullEnumerator<'TKey, 'TValue> (stack, node)
            while enumerator.MoveNext () do yield enumerator.Current
        }

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

        // ## Debug: check internal keys ##

        let rec debugGetIntKeysNode node =
            match node with
            | Int intNode -> debugGetIntKeysInt intNode
            | Leaf _ -> Seq.empty

        and debugGetIntKeysInt node = seq {
            for kv in node.Nodes do
                yield kv.Key
                yield! kv.Value |> debugGetIntKeysNode
        }

        let debugCheckIntKeys () =
            // All the internal node keys should be keys in the actual data
            let excessIntKeys = SortedSet<'TKey>(Comparer)
            for key in debugGetIntKeysNode Root do
                excessIntKeys.Add key |> ignore

            for (kv: KeyValuePair<'TKey, 'TValue>) in enumerateAll Root do
                excessIntKeys.Remove kv.Key |> ignore

            if excessIntKeys.Count > 0 then
                let excessArray = Array.zeroCreate (excessIntKeys.Count)
                excessIntKeys.CopyTo excessArray
                Some <| sprintf "Internal keys not in key set: %A" excessArray

            else None

        // ## Debug: check depth ##

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
            (Seq.head depths) + 1

        let debugCheckCount () =
            let actualCount = enumerateAll Root |> Seq.length
            if actualCount <> Count then Some <| sprintf "Had count property %d, actual count %d" Count actualCount else None

        let debugCheckDepth () =
            // Count tree leaf nodes and check depth.
            // See https://cs.stackexchange.com/questions/82015/maximum-depth-of-a-b-tree
            let depth = debugGetDepthNode Root
            let leafCount = debugCountLeafNodes Root
            let maxDepth = 1 + (Math.Log (leafCount, (float B) / 2.0) |> Math.Ceiling |> int)
            if depth > maxDepth then Some <| sprintf "Required max depth = %d, found %d" maxDepth depth
            elif depth <> Depth then Some <| sprintf "Calculated depth = %d, but had recorded %d" depth Depth
            else None

        // ## Debug: formatted print ##

        let rec debugPrintNode prefix (sb: StringBuilder) node =
            match node with
            | Int intNode -> debugPrintInt prefix sb intNode
            | Leaf leafNode -> debugPrintLeaf prefix sb leafNode

        and debugPrintInt prefix sb node =
            let keyStrings = node.Nodes |> Array.map (fun kv -> $"{kv.Key}")
            let keyStringMaxLength = keyStrings |> Seq.fold (fun max s -> Math.Max (max, s.Length)) 2
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

        // ## Search ##

        let rec findInNode key node (result: outref<'TValue>) =
            match node with
            | Int intNode -> findInInt key intNode &result
            | Leaf leafNode -> findInLeaf key leafNode &result

        and findInInt key node result =
            let index = findIndexInInt key node
            let searchNode =
                if index = node.Nodes.Length then node.Last else node.Nodes[index].Value

            findInNode key searchNode &result

        and findInLeaf key node result =
            let mutable index = 0
            match findIndexInLeaf key node &index with
            | true ->
                result <- node.Values[index].Value
                true
            | _ -> false

        // ## First item (lowest) ##

        let rec firstInNode node =
            match node with
            | Int intNode ->
                if intNode.Nodes.Length = 0 then firstInNode intNode.Last
                else intNode.Nodes[0].Value |> firstInNode
            | Leaf leafNode -> leafNode.Values[0]

        // ## Insert ##

        let rec insertInNode key value node (result: outref<NodeInsertion<'TKey, 'TValue> >) =
            match node with
            | Int intNode -> insertInInt key value intNode &result
            | Leaf leafNode -> insertInLeaf key value leafNode &result

        and insertInLeaf key value node (result: outref<NodeInsertion<'TKey, 'TValue> >) =
            let mutable index = 0
            let isExactMatch = findIndexInLeaf key node &index
            if isExactMatch then
                // This is a straightforward value replacement and never needs splitting
                let newValues =
                    ArrayUtil.arraySplice1 index 1 (KeyValuePair(key, value)) (ReadOnlySpan node.Values)

                result <- newValues |> LeafNode |> Leaf |> Updated

            else
                // Sanity check
                if node.Values.Length >= B then
                    failwithf "Insert %A: found leaf node with %d values (B=%d)" key node.Values.Length B

                // Create the combined array
                let newValues =
                    ArrayUtil.arraySplice1 index 0 (KeyValuePair(key, value)) (ReadOnlySpan node.Values)

                if newValues.Length < B then
                    result <- newValues |> LeafNode |> Leaf |> Inserted
                else
                    // This is too big, split it
                    // Note an F# array slice is *inclusive of the last index*
                    let newValues1 = newValues[..(lengthOfSplitLeafNode - 1)]
                    let newValues2 = newValues[lengthOfSplitLeafNode..]
                    result <- Split (newValues1 |> LeafNode |> Leaf, newValues2[0].Key, newValues2 |> LeafNode |> Leaf)

        and insertInInt key value node (result: outref<NodeInsertion<'TKey, 'TValue> >) =
            // Do the recursive insert
            let index = findIndexInInt key node
            let mutable update = Unchecked.defaultof< NodeInsertion<'TKey, 'TValue> >
            if index = node.Nodes.Length then insertInNode key value node.Last &update
            else insertInNode key value node.Nodes[index].Value &update

            match update with
            | Inserted u ->
                // This can be inserted without any resize of the current node and no change in key
                if index = node.Nodes.Length then
                    result <- IntNode (node.Nodes, u) |> Int |> Inserted
                else
                    let newItem = KeyValuePair(node.Nodes[index].Key, u)
                    let newNodes =
                        ArrayUtil.arraySplice1 index 1 newItem (ReadOnlySpan node.Nodes)

                    result <- IntNode (newNodes, node.Last) |> Int |> Inserted

            | Updated u ->
                // Basically the same case as the previous one
                if index = node.Nodes.Length then
                    result <- IntNode (node.Nodes, u) |> Int |> Updated
                else
                    let newItem = KeyValuePair(node.Nodes[index].Key, u)
                    let newNodes =
                        ArrayUtil.arraySplice1 index 1 newItem (ReadOnlySpan node.Nodes)

                    result <- IntNode (newNodes, node.Last) |> Int |> Updated

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
                            ArrayUtil.arraySplice1 index 0 newItem (ReadOnlySpan node.Nodes)
                            
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
                            ArrayUtil.arraySplice2 index 1 newItem1 newItem2 (ReadOnlySpan node.Nodes)

                        IntNode (newNodes, node.Last)

                if updated.Nodes.Length < B then
                    // No further splitting is required
                    result <- updated |> Int |> Inserted

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
                    result <- Split (head |> Int, tailKey, tail |> Int)

        // ## Delete ##

        let rec deleteFromNode key (left, middle, right) (result: outref<NodeDeletion<'TKey, 'TValue> >) =
            match (left, middle, right) with
            | (None, Int m, None) -> deleteFromInt key (None, m, None) &result
            | (None, Int m, Some (Int r)) -> deleteFromInt key (None, m, Some r) &result
            | (Some (Int l), Int m, None) -> deleteFromInt key (Some l, m, None) &result
            | (Some (Int l), Int m, Some (Int r)) -> deleteFromInt key (Some l, m, Some r) &result
            | (None, Leaf m, None) -> deleteFromLeaf key (None, m, None) &result
            | (None, Leaf m, Some (Leaf r)) -> deleteFromLeaf key (None, m, Some r) &result
            | (Some (Leaf l), Leaf m, None) -> deleteFromLeaf key (Some l, m, None) &result
            | (Some (Leaf l), Leaf m, Some (Leaf r)) -> deleteFromLeaf key (Some l, m, Some r) &result
            | _ -> failwithf "Left-middle-right mismatch: %A; %A; %A" left middle right

        and deleteFromInt key (left, node, right) (result: outref<NodeDeletion<'TKey, 'TValue> >) =
            let index = findIndexInInt key node
            let thisLeft = if index = 0 then None else Some (node.Nodes[index - 1].Value)
            let thisNode = if index = node.Nodes.Length then node.Last else node.Nodes[index].Value
            let thisRight =
                if index = node.Nodes.Length then None
                elif index = node.Nodes.Length - 1 then Some (node.Last)
                else Some (node.Nodes[index + 1].Value)

            let mutable update = Unchecked.defaultof< NodeDeletion<'TKey, 'TValue> >
            deleteFromNode key (thisLeft, thisNode, thisRight) &update
            match update with
            | NotPresent ->
                result <- NotPresent

            | Kept (minKey, keptNode) ->
                if index = 0 then
                    let newNodes =
                        ArrayUtil.arraySplice1 0 1 (KeyValuePair (node.Nodes[0].Key, keptNode)) (ReadOnlySpan node.Nodes)

                    postDeleteFromInt (Some minKey) (left, IntNode (newNodes, node.Last), right) &result

                elif index = node.Nodes.Length then
                    let newNodes =
                        ArrayUtil.arraySplice1 (index - 1) 1 (KeyValuePair (minKey, node.Nodes[index - 1].Value)) (ReadOnlySpan node.Nodes)

                    postDeleteFromInt None (left, IntNode (newNodes, keptNode), right) &result

                else
                    // We replaced a node somewhere within the list.
                    let newNode1 = KeyValuePair (minKey, node.Nodes[index - 1].Value)
                    let newNode2 = KeyValuePair (node.Nodes[index].Key, keptNode)
                    let newNodes =
                        ArrayUtil.arraySplice2 (index - 1) 2 newNode1 newNode2 (ReadOnlySpan node.Nodes)

                    postDeleteFromInt None (left, IntNode (newNodes, node.Last), right) &result

            | BorrowedLeft (bLeft, bMiddleKey, bMiddle) ->
                if index = 0 then failwith "No left sibling to borrow from"
                elif index = node.Nodes.Length then
                    let newNodes =
                        ArrayUtil.arraySplice1 (index - 1) 1 (KeyValuePair (bMiddleKey, bLeft)) (ReadOnlySpan node.Nodes)

                    postDeleteFromInt None (left, IntNode (newNodes, bMiddle), right) &result

                else
                    let newNode1 = KeyValuePair (bMiddleKey, bLeft)
                    let newNode2 = KeyValuePair (node.Nodes[index].Key, bMiddle)
                    let newNodes =
                        ArrayUtil.arraySplice2 (index - 1) 2 newNode1 newNode2 (ReadOnlySpan node.Nodes)

                    postDeleteFromInt None (left, IntNode (newNodes, node.Last), right) &result

            | BorrowedRight (bMiddleKey, bMiddle, bRightKey, bRight) ->
                if index = node.Nodes.Length then failwith "No right sibling to borrow from"
                elif index = 0 && node.Nodes.Length = 1 then
                    let newNodes = [| KeyValuePair (bRightKey, bMiddle) |]
                    postDeleteFromInt (Some bMiddleKey) (left, IntNode (newNodes, bRight), right) &result

                elif index = node.Nodes.Length - 1 then
                    let newNode1 = KeyValuePair (bMiddleKey, node.Nodes[index - 1].Value)
                    let newNode2 = KeyValuePair (bRightKey, bMiddle)
                    let newNodes =
                        ArrayUtil.arraySplice2 (index - 1) 2 newNode1 newNode2 (ReadOnlySpan node.Nodes)

                    postDeleteFromInt None (left, IntNode (newNodes, bRight), right) &result

                 elif index = 0 then
                    let newNode1 = KeyValuePair (bRightKey, bMiddle)
                    let newNode2 = KeyValuePair (node.Nodes[index + 1].Key, bRight)
                    let newNodes =
                        ArrayUtil.arraySplice2 index 2 newNode1 newNode2 (ReadOnlySpan node.Nodes)

                    postDeleteFromInt (Some bMiddleKey) (left, IntNode (newNodes, node.Last), right) &result

                else
                    let newNode1 = KeyValuePair (bMiddleKey, node.Nodes[index - 1].Value)
                    let newNode2 = KeyValuePair (bRightKey, bMiddle)
                    let newNode3 = KeyValuePair (node.Nodes[index + 1].Key, bRight)
                    let newNodes =
                        ArrayUtil.arraySplice3 (index - 1) 3 newNode1 newNode2 newNode3 (ReadOnlySpan node.Nodes)

                    postDeleteFromInt None (left, IntNode (newNodes, node.Last), right) &result

            | MergedLeft mLeft ->
                if index = 0 then failwith "No left sibling to borrow from"
                elif index = node.Nodes.Length then
                    let newNodes = ArrayUtil.arrayDelete (node.Nodes.Length - 1) 1 (ReadOnlySpan node.Nodes)
                    postDeleteFromInt None (left, IntNode (newNodes, mLeft), right) &result

                else
                    let newNodes =
                        ArrayUtil.arraySplice1 (index - 1) 2 (KeyValuePair (node.Nodes[index].Key, mLeft)) (ReadOnlySpan node.Nodes)
                    postDeleteFromInt None (left, IntNode (newNodes, node.Last), right) &result

            | MergedRight (mRightKey, mRight) ->
                if index = node.Nodes.Length then failwith "No right sibling to borrow from"
                elif index = 0 && node.Nodes.Length = 1 then
                    // Here I need to generate an empty internal node with a Left pointer only.
                    // This isn't valid in any complete tree but should be okay transiently (it'll be merged again)
                    postDeleteFromInt (Some mRightKey) (left, IntNode ([||], mRight), right) &result

                elif index = 0 then
                    let newNodes =
                        ArrayUtil.arraySplice1 0 2 (KeyValuePair (node.Nodes[index + 1].Key, mRight)) (ReadOnlySpan node.Nodes)
                    postDeleteFromInt (Some mRightKey) (left, IntNode (newNodes, node.Last), right) &result

                elif index = node.Nodes.Length - 1 then
                    let newNodes =
                        ArrayUtil.arraySplice1 (index - 1) 2 (KeyValuePair (mRightKey, node.Nodes[index - 1].Value)) (ReadOnlySpan node.Nodes)

                    postDeleteFromInt None (left, IntNode (newNodes, mRight), right) &result

                else
                    let newNode1 = KeyValuePair (mRightKey, node.Nodes[index - 1].Value)
                    let newNode2 = KeyValuePair (node.Nodes[index + 1].Key, mRight)
                    let newNodes =
                        ArrayUtil.arraySplice2 (index - 1) 3 newNode1 newNode2 (ReadOnlySpan node.Nodes)

                    postDeleteFromInt None (left, IntNode (newNodes, node.Last), right) &result

            | Deleted ->
                if index = 0 && node.Nodes.Length = 0 then
                    result <- Deleted

                elif index = node.Nodes.Length then
                    let newNodes = ArrayUtil.arrayDelete (node.Nodes.Length - 1) 1 (ReadOnlySpan node.Nodes)
                    postDeleteFromInt None (left, IntNode (newNodes, node.Nodes[node.Nodes.Length - 1].Value), right) &result

                else
                    let newNodes = ArrayUtil.arrayDelete index 1 (ReadOnlySpan node.Nodes)
                    postDeleteFromInt None (left, IntNode (newNodes, node.Last), right) &result

        and postDeleteFromInt maybeMinKey (left, node, right) (result: outref<NodeDeletion<'TKey, 'TValue> >) =
            let resolveMinKey () =
                match maybeMinKey with
                | Some m -> m
                | None -> (node |> Int |> firstInNode).Key

            // Work out whether or not this node is still big enough and
            // if not, borrow/merge as appropriate.
            match (left, right) with
            | _ when node.Nodes.Length >= lengthOfSplitIntNode ->
                // TODO common case -- optimisation: avoid needing to fetch `maybeMinKey`?
                result <- Kept (resolveMinKey (), node |> Int)

            | (Some l, _) when l.Nodes.Length > lengthOfSplitIntNode ->
                // We can grab a value from the node on the left to fill this one out:
                let newMiddleNodes = ArrayUtil.arraySplice1 0 0 (KeyValuePair (resolveMinKey (), l.Last)) (ReadOnlySpan node.Nodes)
                let newLeftLast = l.Nodes[l.Nodes.Length - 1]
                let newLeftNodes = ArrayUtil.arrayDelete (l.Nodes.Length - 1) 1 (ReadOnlySpan l.Nodes)
                result <- BorrowedLeft (
                    IntNode (newLeftNodes, newLeftLast.Value) |> Int,
                    newLeftLast.Key,
                    IntNode (newMiddleNodes, node.Last) |> Int)

            | (_, Some r) when r.Nodes.Length > lengthOfSplitIntNode ->
                // We can grab a value from the node on the right to fill this one out:
                let rightMinKey = (firstInNode r.Nodes[0].Value).Key
                let newMiddleNodes = ArrayUtil.arraySplice1 node.Nodes.Length 0 (KeyValuePair (rightMinKey, node.Last)) (ReadOnlySpan node.Nodes)
                let newMiddleLast = r.Nodes[0]
                let newRightNodes = ArrayUtil.arrayDelete 0 1 (ReadOnlySpan r.Nodes)
                result <- BorrowedRight (
                    resolveMinKey (),
                    IntNode (newMiddleNodes, newMiddleLast.Value) |> Int,
                    newMiddleLast.Key,
                    IntNode (newRightNodes, r.Last) |> Int)

            | (Some l, _) ->
                // The left node and this are both minimum size nodes or under and we can merge them
                let minItem = KeyValuePair (resolveMinKey (), l.Last)
                let mergedNodes =
                    ArrayUtil.arraySandwich (ReadOnlySpan l.Nodes) minItem (ReadOnlySpan node.Nodes)

                result <- IntNode (mergedNodes, node.Last) |> Int |> MergedLeft

            | (_, Some r) ->
                // The right node and this are both minimum size nodes or under and we can merge them
                let rightMinItem = KeyValuePair ((firstInNode r.Nodes[0].Value).Key, node.Last)
                let mergedNodes =
                    ArrayUtil.arraySandwich (ReadOnlySpan node.Nodes) rightMinItem (ReadOnlySpan r.Nodes)

                result <- MergedRight (resolveMinKey (), IntNode (mergedNodes, r.Last) |> Int)

            | _ ->
                // This should only occur if this is the only node in the tree.
                result <- Kept (resolveMinKey (), node |> Int)

        and deleteFromLeaf key (left, node, right) (result: outref<NodeDeletion<'TKey, 'TValue> >) =
            let mutable index = 0
            if findIndexInLeaf key node &index then
                let newValues = ArrayUtil.arrayDelete index 1 (ReadOnlySpan node.Values)
                match (left, right) with
                | _ when newValues.Length >= lengthOfSplitLeafNode ->
                    // No borrowing or merging required
                    result <- Kept (newValues[0].Key, newValues |> LeafNode |> Leaf)

                | (Some l, _) when l.Values.Length > lengthOfSplitLeafNode ->
                    // We can grab a value from the node on the left to fill this one out:
                    let newMiddle = ArrayUtil.arraySplice1 0 0 l.Values[l.Values.Length - 1] (ReadOnlySpan newValues)
                    let newLeft = ArrayUtil.arrayDelete (l.Values.Length - 1) 1 (ReadOnlySpan l.Values)
                    result <- BorrowedLeft (newLeft |> LeafNode |> Leaf, newMiddle[0].Key, newMiddle |> LeafNode |> Leaf)

                | (_, Some r) when r.Values.Length > lengthOfSplitLeafNode ->
                    // We can grab a value from the node on the right to fill this one out:
                    let newMiddle = ArrayUtil.arraySplice1 newValues.Length 0 r.Values[0] (ReadOnlySpan newValues)
                    let newRight = ArrayUtil.arrayDelete 0 1 (ReadOnlySpan r.Values)
                    result <- BorrowedRight (newMiddle[0].Key, newMiddle |> LeafNode |> Leaf, newRight[0].Key, newRight |> LeafNode |> Leaf)

                | (Some l, _) ->
                    // The left node and this are both minimum size nodes or under and we can merge them
                    let merged = ArrayUtil.arraySpliceX 0 0 (ReadOnlySpan l.Values) (ReadOnlySpan newValues)
                    result <- MergedLeft (merged |> LeafNode |> Leaf)

                | (_, Some r) ->
                    // The right node and this are both minimum size nodes or under and we can merge them
                    let merged = ArrayUtil.arraySpliceX newValues.Length 0 (ReadOnlySpan r.Values) (ReadOnlySpan newValues)
                    result <- MergedRight (merged[0].Key, merged |> LeafNode |> Leaf)

                | _ ->
                    // This should only occur if this is the only node in the tree.
                    result <-
                        if newValues.Length = 0 then Deleted
                        else Kept (newValues[0].Key, newValues |> LeafNode |> Leaf)

            else
                result <- NotPresent

        // ## Internal methods ##

        member internal _.CreateEnumeratorFrom key =
            // Construct a stack for the partial enumerator
            let stack = ArrayPool< NodeStackFrame<'TKey, 'TValue> >.Shared.Rent Depth
            stack[0].Node <- Root

            let mutable stackIndex = 0
            let mutable notFound = true

            while notFound do
                match stack[stackIndex].Node with
                | Int intNode ->
                    let index = findIndexInInt key intNode
                    stack[stackIndex].Index <- index + 1
                    stackIndex <- stackIndex + 1
                    stack[stackIndex].Node <-
                        if index = intNode.Nodes.Length then intNode.Last
                        else intNode.Nodes[index].Value

                | Leaf leafNode ->
                    let mutable index = 0
                    let _ = findIndexInLeaf key leafNode &index
                    stack[stackIndex].Index <- index
                    notFound <- false

            new TreePartialEnumerator<'TKey, 'TValue> (stack, stackIndex)

        // ## Public methods ##

        member _.DebugValidate () =
            match debugCheckCount () with
            | Some err -> Some err
            | None ->
                match debugCheckDepth () with
                | Some err -> Some err
                | None ->
                    match debugCheckIntKeys () with
                    | Some err -> Some err
                    | None ->
                        match debugCheckWidthsNode (Some 0) Root with
                        | Some err -> Some err
                        | None ->
                            match debugValidateNode Root with
                            | NotValid err -> Some err
                            | _ -> None

        member this.Delete key =
            let mutable result = Unchecked.defaultof< NodeDeletion<'TKey, 'TValue> >
            deleteFromNode key (None, Root, None) &result
            match result with
            | NotPresent -> this
            | Kept (_, Int intNode) when intNode.Nodes.Length = 0 ->
                // Reduce the height of the tree by 1
                let depth = if Depth = 1 then 1 else Depth - 1
                Tree (B, Count - 1, depth, Comparer, intNode.Last)
            | Kept (_, node) -> Tree (B, Count - 1, Depth, Comparer, node)
            | Deleted -> Tree (B, Count - 1, Depth, Comparer, emptyLeaf<'TKey, 'TValue>)
            | _ -> failwith "Unhandled delete case" // shouldn't reach this

        member _.EnumerateAll () =
            // Construct a stack for the full enumerator
            let stack = ArrayPool< NodeStackFrame<'TKey, 'TValue> >.Shared.Rent Depth
            stack[0].Index <- 0
            stack[0].Node <- Root
            new TreeFullEnumerator<'TKey, 'TValue> (stack, Root)

        member this.EnumerateFrom key = TreePartialEnumerable<'TKey, 'TValue> (key, this)

        member _.First () = firstInNode Root

        member _.Insert key value =
            let mutable result = Unchecked.defaultof< NodeInsertion<'TKey, 'TValue> >
            insertInNode key value Root &result
            match result with
            | Inserted u -> Tree(B, Count + 1, Depth, Comparer, u)
            | Updated u -> Tree(B, Count, Depth, Comparer, u)
            | Split (head, tailKey, tail) ->
                // Generate a new root node out of this split:
                let newRoot = IntNode ([|KeyValuePair(tailKey, head)|], tail) |> Int
                Tree(B, Count + 1, Depth + 1, Comparer, newRoot)

        member _.TryFind (key, result: outref<'TValue>) = findInNode key Root &result

        // TODO test this. I think I'll need the logic inside for implementing delete properly;
        // also it's useful in its own right
        static member CreateFrom b (cmp: IComparer<'TKey>) (eqCmp: IEqualityComparer<'TKey>) (values: KeyValuePair<'TKey, 'TValue> seq) =
            // Suspicion section: make sure that the values are sorted in order of the
            // given comparer, and remove any with duplicate keys
            let valuesArray = ArrayUtil.sortedAndDistinct cmp eqCmp values

            // Make a suitable root for the tree
            let creations = createSubtree b valuesArray
            let depth, root =
                match creations.Length with
                | 0 -> (1, emptyLeaf<'TKey, 'TValue>)
                | 1 -> (creations[0].Depth, creations[0].Node)
                | _ -> (creations[0].Depth + 1, (creationsToIntNode creations).Node)

            Tree (b, valuesArray.Length, depth, cmp, root)

        override _.ToString () =
            // Debug print
            let sb = StringBuilder ()
            sb.AppendLine $"Tree({B}, " |> ignore
            debugPrintNode "" sb Root
            sb.AppendLine ")" |> ignore
            sb.ToString ()

        interface System.Collections.IEnumerable with
            member this.GetEnumerator () = this.EnumerateAll ()

        interface IEnumerable<KeyValuePair<'TKey, 'TValue> > with
            member this.GetEnumerator () = this.EnumerateAll ()

    // The stack here is leased from the array pool and must be returned on dispose
    and [<AbstractClass>] TreeEnumerator<'TKey, 'TValue>(Stack: NodeStackFrame<'TKey, 'TValue> [], Index: int) =
        let mutable stackIndex = Index
        let mutable current = Unchecked.defaultof< KeyValuePair<'TKey, 'TValue> >
        let mutable isDisposed = false

        member _.Current = current

        member internal _.AssignStack1 node =
            stackIndex <- 0
            Stack[0].Index <- 0
            Stack[0].Node <- node

        member internal _.AssignStackX (frames: ReadOnlySpan< NodeStackFrame<'TKey, 'TValue> >) =
            frames.CopyTo (Stack.AsSpan ())
            stackIndex <- frames.Length - 1

        abstract DisposeInternal : unit -> unit
        default _.DisposeInternal () =
            ArrayPool.Shared.Return Stack

        member _.MoveNext () =
            let mutable found = false
            while not found && stackIndex >= 0 do
                let nodeIndex = Stack[stackIndex].Index
                Stack[stackIndex].Index <- nodeIndex + 1
                match Stack[stackIndex].Node with
                | Int intNode ->
                    if nodeIndex > intNode.Nodes.Length then
                        // I've reached the end of this node and need to go back to
                        // the previous frame of the stack.
                        stackIndex <- stackIndex - 1

                    else
                        // Push the next node onto the stack.
                        stackIndex <- stackIndex + 1
                        Stack[stackIndex].Index <- 0
                        Stack[stackIndex].Node <-
                            if nodeIndex = intNode.Nodes.Length then intNode.Last
                            else intNode.Nodes[nodeIndex].Value

                | Leaf leafNode ->
                    if nodeIndex >= leafNode.Values.Length then
                        // I've reached the end of this node and need to go back to
                        // the previous frame of the stack.
                        stackIndex <- stackIndex - 1

                    else
                        current <- leafNode.Values[nodeIndex]
                        found <- true

            found

        abstract member Reset : unit -> unit

        interface IDisposable with
            member this.Dispose () =
                if not isDisposed then
                    this.DisposeInternal ()
                    isDisposed <- true

        interface System.Collections.IEnumerator with
            member this.Current =
#if DEBUG
                if isDisposed then raise <| ObjectDisposedException (nameof this)
#endif
                current :> obj      

            member this.MoveNext () =
#if DEBUG
                if isDisposed then raise <| ObjectDisposedException (nameof this)
#endif
                this.MoveNext ()

            member this.Reset () =
#if DEBUG
                if isDisposed then raise <| ObjectDisposedException (nameof this)
#endif
                this.Reset ()

        interface IEnumerator< KeyValuePair<'TKey, 'TValue> > with
            member this.Current =
#if DEBUG
                if isDisposed then raise <| ObjectDisposedException (nameof this)
#endif
                current

    and TreeFullEnumerator<'TKey, 'TValue>(Stack: NodeStackFrame<'TKey, 'TValue> [], Root: Node<'TKey, 'TValue>) =
        inherit TreeEnumerator<'TKey, 'TValue>(Stack, 0)

        override this.Reset () = this.AssignStack1 Root

    and TreePartialEnumerator<'TKey, 'TValue>(Stack: NodeStackFrame<'TKey, 'TValue> [], Index: int) =
        inherit TreeEnumerator<'TKey, 'TValue>(Stack, Index)

        // Copy the original stack to another one that I can restore in case of reset:
        let resetStack = ArrayPool< NodeStackFrame<'TKey, 'TValue> >.Shared.Rent (Index + 1)
        do
            let span = ReadOnlySpan Stack
            let slice = span.Slice (start = 0, length = Index + 1)
            slice.CopyTo (resetStack.AsSpan ())

        override _.DisposeInternal () =
            ArrayPool.Shared.Return resetStack
            base.DisposeInternal ()

        override this.Reset () =
            let span = ReadOnlySpan resetStack
            let slice = span.Slice (start = 0, length = Index + 1)
            this.AssignStackX slice

    and TreePartialEnumerable<'TKey, 'TValue>(Key: 'TKey, Tre: Tree<'TKey, 'TValue>) =
        interface System.Collections.IEnumerable with
            member this.GetEnumerator () = (Tre.CreateEnumeratorFrom Key) :> System.Collections.IEnumerator

        interface IEnumerable< KeyValuePair<'TKey, 'TValue> > with
            member this.GetEnumerator () = (Tre.CreateEnumeratorFrom Key) :> IEnumerator< KeyValuePair<'TKey, 'TValue> >

    let bValueFor<'T> = Math.Max (3, 64_000 / sizeof<'T>)

    let create<'TKey, 'TValue> cmp =
        new Tree<'TKey, 'TValue>(bValueFor<'TKey>, 0, 1, cmp, emptyLeaf<'TKey, 'TValue>)

    let createB<'TKey, 'TValue> b cmp =
        if b < 3 then raise <| ArgumentException("b must be at least 3", nameof(b))
        new Tree<'TKey, 'TValue>(b, 0, 1, cmp, emptyLeaf<'TKey, 'TValue>)

    let createFrom<'TKey, 'TValue> cmp eqCmp values =
        Tree<'TKey, 'TValue>.CreateFrom bValueFor<'TKey> cmp eqCmp values

    let createFromB<'TKey, 'TValue> b cmp eqCmp values =
        if b < 3 then raise <| ArgumentException("b must be at least 3", nameof(b))
        Tree<'TKey, 'TValue>.CreateFrom b cmp eqCmp values

    let empty<'TKey, 'TValue> =
        new Tree<'TKey, 'TValue>(bValueFor<'TKey>, 0, 1, Comparer<'TKey>.Default, emptyLeaf<'TKey, 'TValue>)

    let emptyB<'TKey, 'TValue> b =
        if b < 3 then raise <| ArgumentException("b must be at least 3", nameof(b))
        new Tree<'TKey, 'TValue>(b, 0, 1, Comparer<'TKey>.Default, emptyLeaf<'TKey, 'TValue>)

    let debugValidate<'TKey, 'TValue> (tree: Tree<'TKey, 'TValue>) =
        tree.DebugValidate ()

    let delete<'TKey, 'TValue> key (tree: Tree<'TKey, 'TValue>) = tree.Delete key

    let enumerateFrom<'TKey, 'TValue> key (tree: Tree<'TKey, 'TValue>) =
        tree.EnumerateFrom key

    let head<'TKey, 'TValue> (tree: Tree<'TKey, 'TValue>) = tree.First ()

    let insert<'TKey, 'TValue> key value (tree: Tree<'TKey, 'TValue>) =
        tree.Insert key value

    let tryFind<'TKey, 'TValue> key (tree: Tree<'TKey, 'TValue>) =
        let mutable result = Unchecked.defaultof<'TValue>
        let found = tree.TryFind (key, &result)
        if found then Some result else None

