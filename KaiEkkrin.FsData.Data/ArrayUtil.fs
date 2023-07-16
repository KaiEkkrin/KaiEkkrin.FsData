namespace KaiEkkrin.FsData.Data

open System
open System.Collections.Generic

module ArrayUtil =
    // Deletes `deleteCount` items from the array at the index and
    // inserts `newItem` there.
    let arraySplice1<'T> index deleteCount newItem (array: 'T []) =
        if index < 0 || index > array.Length then
            raise <| ArgumentException ("Index is outside the bounds of the array", nameof(index))

        if deleteCount < 0 then raise <| ArgumentException ("Cannot delete a negative count", nameof(index))
        if (index + deleteCount) > array.Length then
            raise <| ArgumentException ("Deletion goes beyond the bounds of the array", nameof(deleteCount))

        let newArray = Array.zeroCreate<'T> (array.Length + 1 - deleteCount)

        // Copy the head of the array
        if index > 0 then
            Array.Copy(
                sourceArray = array,
                destinationArray = newArray,
                length = index
            )

        // Insert the new item
        newArray[index] <- newItem

        // Copy the tail of the array
        let tailStart = index + deleteCount
        let tailLength = array.Length - tailStart
        //raise <| InvalidOperationException (sprintf "index = %d, deleteCount = %d, array.Length = %d, newArray.Length = %d, tailStart = %d, tailLength = %d" index deleteCount array.Length newArray.Length tailStart tailLength)
        if tailLength > 0 then
            Array.Copy(
                sourceArray = array,
                sourceIndex = tailStart,
                destinationArray = newArray,
                destinationIndex = index + 1,
                length = tailLength
            )

        newArray

    // Deletes `deleteCount` items from the array at the index and
    // inserts `newItems` there.
    let arraySpliceX<'T> index deleteCount (newItems: 'T []) (array: 'T []) =
        if index < 0 || index > array.Length then
            raise <| ArgumentException ("Index is outside the bounds of the array", nameof(index))

        if deleteCount < 0 then raise <| ArgumentException ("Cannot delete a negative count", nameof(index))
        if (index + deleteCount) > array.Length then
            raise <| ArgumentException ("Deletion goes beyond the bounds of the array", nameof(deleteCount))

        let newArray = Array.zeroCreate<'T> (array.Length + newItems.Length - deleteCount)

        // Copy the head of the array
        if index > 0 then
            Array.Copy(
                sourceArray = array,
                destinationArray = newArray,
                length = index
            )

        // Insert the new items
        if newItems.Length > 0 then
            Array.Copy(
                sourceArray = newItems,
                sourceIndex = 0,
                destinationArray = newArray,
                destinationIndex = index,
                length = newItems.Length
            )

        // Copy the tail of the array
        let tailStart = index + deleteCount
        let tailLength = array.Length - tailStart
        if tailLength > 0 then
            Array.Copy(
                sourceArray = array,
                sourceIndex = tailStart,
                destinationArray = newArray,
                destinationIndex = index + newItems.Length,
                length = tailLength
            )

        newArray

    // Breaks an array into an array of chunks of size between `minChunkSize` and `maxChunkSize` inclusive,
    // if it can.
    let splitIntoChunks<'T> minChunkSize maxChunkSize (array: 'T []) =
        if minChunkSize < 1 || minChunkSize > maxChunkSize then failwithf "Invalid chunk sizes: %d, %d" minChunkSize maxChunkSize

        // Look for a chunk size such that we don't end up with an overly-small remainder that we can't fill.
        // Emits the array of chunk sizes to use.
        let rec lookForSplit sz =
            if sz < minChunkSize then
                failwithf "Can't find a valid split for a %d length array to chunk sizes %d, %d" array.Length minChunkSize maxChunkSize
            else
                let (fullChunks, leftover) = Math.DivRem (array.Length, sz)
                if leftover = 0 then List.init fullChunks (fun _ -> sz)
                else
                    let chunkSizes = List.init (fullChunks + 1) (fun i -> if i = 0 then leftover else sz)
                    if leftover >= minChunkSize then chunkSizes
                    else
                        // Walk through the list borrowing enough from the next item to make each item
                        // up to min size if it isn't yet. If I reach the end and I still have something
                        // left to borrow, I've failed to generate a good list.
                        // This function will also reverse the chunk size list, but that doesn't matter
                        let (fixedSizes, remainder) =
                            chunkSizes
                            |> List.fold (fun (l, debt) ch ->
                                let withoutDebt = ch - debt
                                if withoutDebt >= minChunkSize
                                    then (withoutDebt::l, 0)
                                    else (minChunkSize::l, minChunkSize - withoutDebt)
                            ) ([], 0)

                        if remainder = 0 then fixedSizes else lookForSplit (sz - 1)

        if array.Length <= maxChunkSize then [|array|]
        else
            let chunkSizes = lookForSplit maxChunkSize
            Array.unfold (fun (i, l) ->
                match l with
                | sz::szs -> Some (array[i..(i + sz - 1)], (i + sz, szs))
                | [] -> None
            ) (0, chunkSizes)

    // Sorts in ascending order of keys (using the given comparer), and removes any duplicates
    // (leaving in the element that was originally the latest in the list)
    // resulting in output that can go through tree creation safely.
    // Probably a bit slow. Hopefully not outrageously so
    let sortedAndDistinct<'TKey, 'TValue> (cmp: IComparer<'TKey>) (eqCmp: IEqualityComparer<'TKey>) (sequence: KeyValuePair<'TKey, 'TValue> seq) =
        // This should get rid of old values for duplicate keys quickly
        let dictionary =
            match sequence with
            | :? IReadOnlyCollection<KeyValuePair<'TKey, 'TValue> > as collection ->
                Dictionary<'TKey, 'TValue>(collection.Count, eqCmp)
            | _ -> Dictionary<'TKey, 'TValue>(eqCmp)

        for kv in sequence do
            dictionary[kv.Key] <- kv.Value

        // Create and sort the array
        let array = Array.zeroCreate<KeyValuePair<'TKey, 'TValue> >(dictionary.Count)
        dictionary |> Seq.iteri (fun i kv -> array[i] <- kv)
        array |> Array.sortInPlaceWith (fun a b -> cmp.Compare (a.Key, b.Key)) |> ignore
        array

