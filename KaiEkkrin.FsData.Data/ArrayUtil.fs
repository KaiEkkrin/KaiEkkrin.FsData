namespace KaiEkkrin.FsData.Data

open System

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
