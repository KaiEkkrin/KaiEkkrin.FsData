namespace KaiEkkrin.FsData.Data

open System
open System.Buffers
open System.Collections.Generic

module ArrayUtil =
    // Deletes `deleteCount` items from the array at the index.
    let arrayDelete<'T> index deleteCount (array: ReadOnlySpan<'T>) =
        let newArray = Array.zeroCreate<'T> (array.Length - deleteCount)
        let newSpan = newArray.AsSpan ()

        // Copy the head of the array
        if index > 0 then
            let arrayHead = array.Slice (0, index)
            arrayHead.CopyTo newSpan

        // Copy the tail of the array
        let tailStart = index + deleteCount
        let tailLength = array.Length - tailStart
        if tailLength > 0 then
            let arrayTail = array.Slice (tailStart, tailLength)
            let newTail = newSpan.Slice (index, tailLength)
            arrayTail.CopyTo newTail

        newArray

    // Sandwiches `newItem` in between `array1` and `array2`.
    let arraySandwich<'T> (array1: ReadOnlySpan<'T>) newItem (array2: ReadOnlySpan<'T>) =
        let newArray = Array.zeroCreate<'T> (array1.Length + array2.Length + 1)
        let newSpan = newArray.AsSpan ()

        array1.CopyTo newSpan
        newSpan[array1.Length] <- newItem

        let tailSpan = newSpan.Slice (array1.Length + 1)
        array2.CopyTo tailSpan

        newArray

    // Deletes `deleteCount` items from the array at the index and
    // inserts `newItem` there.
    let arraySplice1<'T> index deleteCount newItem (array: ReadOnlySpan<'T>) =
#if DEBUG
        if index < 0 || index > array.Length then
            raise <| ArgumentException ("Index is outside the bounds of the array", nameof(index))

        if deleteCount < 0 then raise <| ArgumentException ("Cannot delete a negative count", nameof(index))
        if (index + deleteCount) > array.Length then
            raise <| ArgumentException ("Deletion goes beyond the bounds of the array", nameof(deleteCount))
#endif

        let newArray = Array.zeroCreate<'T> (array.Length + 1 - deleteCount)
        let newSpan = newArray.AsSpan ()

        // Copy the head of the array
        if index > 0 then
            let arrayHead = array.Slice (0, index)
            arrayHead.CopyTo newSpan

        // Insert the new item
        newSpan[index] <- newItem

        // Copy the tail of the array
        let tailStart = index + deleteCount
        let tailLength = array.Length - tailStart
        //raise <| InvalidOperationException (sprintf "index = %d, deleteCount = %d, array.Length = %d, newArray.Length = %d, tailStart = %d, tailLength = %d" index deleteCount array.Length newArray.Length tailStart tailLength)
        if tailLength > 0 then
            let arrayTail = array.Slice (tailStart, tailLength)
            let newTail = newSpan.Slice (index + 1, tailLength)
            arrayTail.CopyTo newTail

        newArray

    // Deletes `deleteCount` items from the array at the index and
    // inserts `newItem1` and `newItem2` there.
    let arraySplice2<'T> index deleteCount newItem1 newItem2 (array: ReadOnlySpan<'T>) =
#if DEBUG
        if index < 0 || index > array.Length then
            raise <| ArgumentException ("Index is outside the bounds of the array", nameof(index))

        if deleteCount < 0 then raise <| ArgumentException ("Cannot delete a negative count", nameof(index))
        if (index + deleteCount) > array.Length then
            raise <| ArgumentException ("Deletion goes beyond the bounds of the array", nameof(deleteCount))
#endif

        let newArray = Array.zeroCreate<'T> (array.Length + 2 - deleteCount)
        let newSpan = newArray.AsSpan ()

        // Copy the head of the array
        if index > 0 then
            let arrayHead = array.Slice (0, index)
            arrayHead.CopyTo newSpan

        // Insert the new items
        newSpan[index] <- newItem1
        newSpan[index + 1] <- newItem2

        // Copy the tail of the array
        let tailStart = index + deleteCount
        let tailLength = array.Length - tailStart
        //raise <| InvalidOperationException (sprintf "index = %d, deleteCount = %d, array.Length = %d, newArray.Length = %d, tailStart = %d, tailLength = %d" index deleteCount array.Length newArray.Length tailStart tailLength)
        if tailLength > 0 then
            let arrayTail = array.Slice (tailStart, tailLength)
            let newTail = newSpan.Slice (index + 2, tailLength)
            arrayTail.CopyTo newTail

        newArray

    // Deletes `deleteCount` items from the array at the index and
    // inserts `newItem1`, `newItem2` and `newItem3` there.
    let arraySplice3<'T> index deleteCount newItem1 newItem2 newItem3 (array: ReadOnlySpan<'T>) =
#if DEBUG
        if index < 0 || index > array.Length then
            raise <| ArgumentException ("Index is outside the bounds of the array", nameof(index))

        if deleteCount < 0 then raise <| ArgumentException ("Cannot delete a negative count", nameof(index))
        if (index + deleteCount) > array.Length then
            raise <| ArgumentException ("Deletion goes beyond the bounds of the array", nameof(deleteCount))
#endif

        let newArray = Array.zeroCreate<'T> (array.Length + 3 - deleteCount)
        let newSpan = newArray.AsSpan ()

        // Copy the head of the array
        if index > 0 then
            let arrayHead = array.Slice (0, index)
            arrayHead.CopyTo newSpan

        // Insert the new items
        newSpan[index] <- newItem1
        newSpan[index + 1] <- newItem2
        newSpan[index + 2] <- newItem3

        // Copy the tail of the array
        let tailStart = index + deleteCount
        let tailLength = array.Length - tailStart
        //raise <| InvalidOperationException (sprintf "index = %d, deleteCount = %d, array.Length = %d, newArray.Length = %d, tailStart = %d, tailLength = %d" index deleteCount array.Length newArray.Length tailStart tailLength)
        if tailLength > 0 then
            let arrayTail = array.Slice (tailStart, tailLength)
            let newTail = newSpan.Slice (index + 3, tailLength)
            arrayTail.CopyTo newTail

        newArray

    // Deletes `deleteCount` items from the array at the index and
    // inserts `newItems` there.
    let arraySpliceX<'T> index deleteCount (newItems: ReadOnlySpan<'T>) (array: ReadOnlySpan<'T>) =
#if DEBUG
        if index < 0 || index > array.Length then
            raise <| ArgumentException ("Index is outside the bounds of the array", nameof(index))

        if deleteCount < 0 then raise <| ArgumentException ("Cannot delete a negative count", nameof(index))
        if (index + deleteCount) > array.Length then
            raise <| ArgumentException ("Deletion goes beyond the bounds of the array", nameof(deleteCount))
#endif

        let newArray = Array.zeroCreate<'T> (array.Length + newItems.Length - deleteCount)
        let newSpan = newArray.AsSpan ()

        // Copy the head of the array
        if index > 0 then
            let arrayHead = array.Slice (0, index)
            arrayHead.CopyTo newSpan

        // Insert the new items
        if newItems.Length > 0 then
            let newInsert = newSpan.Slice (index, newItems.Length)
            newItems.CopyTo newInsert

        // Copy the tail of the array
        let tailStart = index + deleteCount
        let tailLength = array.Length - tailStart
        if tailLength > 0 then
            let arrayTail = array.Slice (tailStart, tailLength)
            let newTail = newSpan.Slice (index + newItems.Length, tailLength)
            arrayTail.CopyTo newTail

        newArray

    // Breaks an array into an array of chunks of size between `minChunkSize` and `maxChunkSize` inclusive,
    // if it can.
    let splitIntoChunks<'T> minChunkSize maxChunkSize (array: 'T []) =
#if DEBUG
        if minChunkSize < 1 || minChunkSize > maxChunkSize then failwithf "Invalid chunk sizes: %d, %d" minChunkSize maxChunkSize
#endif

        // Look for a chunk size such that we don't end up with an overly-small remainder that we can't fill.
        // Emits the array of chunk sizes to use.
        // This method is important to get good performance for tree create, so I'm going to do nasty things to it here :)
        let rec lookForSplit sz (sizes: int[]) =
            if sz < minChunkSize then
                failwithf "Can't find a valid split for a %d length array to chunk sizes %d, %d" array.Length minChunkSize maxChunkSize
            else
                let (fullChunks, leftover) = Math.DivRem (array.Length, sz)
                if leftover = 0 then
                    for i in 0..(fullChunks - 1) do
                        sizes[i] <- sz
                    fullChunks

                else
                    let chunkCount = fullChunks + 1
                    sizes[0] <- leftover
                    for i in 1..fullChunks do
                        sizes[i] <- sz

                    if leftover >= minChunkSize then chunkCount
                    else
                        // Walk through the list borrowing enough from the next item to make each item
                        // up to min size if it isn't yet. If I reach the end and I still have something
                        // left to borrow, I've failed to generate a good list.
                        // This function will also reverse the chunk size list, but that doesn't matter
                        let mutable debt = 0
                        let mutable index = 0
                        while index < chunkCount && (index = 0 || debt > 0) do
                            let withoutDebt = sizes[index] - debt
                            if withoutDebt >= minChunkSize then
                                sizes[index] <- withoutDebt
                                debt <- 0
                            else
                                sizes[index] <- minChunkSize
                                debt <- minChunkSize - withoutDebt

                            index <- index + 1

                        if debt = 0 then chunkCount else lookForSplit (sz - 1) sizes

        if array.Length <= maxChunkSize then [|array|]
        else
            let maxChunkCount = Common.divCeil array.Length minChunkSize
            let chunkSizes = ArrayPool<int>.Shared.Rent maxChunkCount
            try
                let chunkCount = lookForSplit maxChunkSize chunkSizes
                let chunks = Array.zeroCreate<'T[]> chunkCount
                let mutable arrayIndex = 0
                for chunkIndex in 0..(chunkCount - 1) do
                    let sz = chunkSizes[chunkIndex]
                    chunks[chunkIndex] <- array[arrayIndex..(arrayIndex + sz - 1)]
                    arrayIndex <- arrayIndex + sz

                chunks

            finally
                ArrayPool<int>.Shared.Return chunkSizes

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
        let mutable i = 0
        for kv in dictionary do
            array[i] <- kv
            i <- i + 1

        let _ = Array.sortInPlaceWith (fun (a: KeyValuePair<'TKey, 'TValue>) b -> cmp.Compare (a.Key, b.Key)) array
        array

