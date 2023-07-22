using System.Buffers;

namespace KaiEkkrin.FsData.Core;

/// <summary>
/// Array utilities.
/// </summary>
public static class ArrayUtil
{
    /// <summary>
    /// Returns a new array with `deleteCount` items removed from the span at the index and `newItem`
    /// inserted there.
    /// </summary>
    public static T[] ArraySplice1<T>(int index, int deleteCount, T newItem, ReadOnlySpan<T> span)
    {
        var newArray = new T[span.Length + 1 - deleteCount];
        var newSpan = newArray.AsSpan();

        // Copy the head of the array
        if (index > 0)
        {
            span[..index].CopyTo(newSpan);
        }

        // Insert the new item
        newArray[index] = newItem;

        // Copy the tail of the array
        var tailStart = index + deleteCount;
        var tailLength = span.Length - tailStart;
        if (tailLength > 0)
        {
            span[tailStart..].CopyTo(newSpan[(index + 1)..]);
        }

        return newArray;
    }

    /// <summary>
    /// Returns a new array with `deleteCount` items removed from `span` starting from `index`, then `newItems`
    /// inserted there.
    /// </summary>
    public static T[] ArraySpliceX<T>(int index, int deleteCount, ReadOnlySpan<T> newItems, ReadOnlySpan<T> span)
    {
        var newArray = new T[span.Length + newItems.Length - deleteCount];
        var newSpan = newArray.AsSpan();

        // Copy the head of the array
        if (index > 0)
        {
            span[..index].CopyTo(newSpan);
        }

        // Insert the new items
        if (newItems.Length > 0)
        {
            newItems.CopyTo(newSpan[index..]);
        }

        // Copy the tail of the array
        var tailStart = index + deleteCount;
        var tailLength = span.Length - tailStart;
        if (tailLength > 0)
        {
            span[tailStart..].CopyTo(newSpan[(index + newItems.Length)..]);
        }

        return newArray;
    }

    /// <summary>
    /// Sorts in ascending order of keys (using the given comparer), keeping the last value for each
    /// duplicated key as determined by the original order.
    /// </summary>
    public static KeyValuePair<TKey, TValue>[] SortedAndDistinct<TKey, TValue>(
        IComparer<TKey> comparer, IEqualityComparer<TKey> equalityComparer, IEnumerable<KeyValuePair<TKey, TValue>> keyValuePairs)
        where TKey : notnull
    {
        // This should get rid of old values for duplicate keys quickly
        // TODO try directly initialising the dictionary with the constructor --
        // will it end up with the correct values (the last for each key), or the wrong ones?
        var dictionary = keyValuePairs is IReadOnlyCollection<KeyValuePair<TKey, TValue>> collection
            ? new Dictionary<TKey, TValue>(collection.Count, equalityComparer)
            : new Dictionary<TKey, TValue>(equalityComparer);

        foreach (var (key, value) in keyValuePairs)
        {
            dictionary[key] = value;
        }

        // Create and sort the array
        var array = dictionary.ToArray();
        Array.Sort(array, (a, b) => comparer.Compare(a.Key, b.Key));
        return array;
    }

    /// <summary>
    /// Breaks an array into an array of chunks between `minChunkSize` and `maxChunkSize` inclusive, if it can.
    /// </summary>
    public static T[][] SplitIntoChunks<T>(int minChunkSize, int maxChunkSize, T[] array)
    {
        if (minChunkSize < 1 || minChunkSize > maxChunkSize) throw new ArgumentOutOfRangeException(nameof(minChunkSize));

        if (array.Length <= maxChunkSize) return new[] { array };

        // Look for a chunk size such that we don't end up with an overly-small remainder that we can't fill.
        // Emits the array of chunk sizes to use.
        var maxChunkCount = Common.DivCeil(array.Length, minChunkSize);
        var chunkSizes = ArrayPool<int>.Shared.Rent(maxChunkCount);
        try
        {
            if (!TryFindSplit(minChunkSize, maxChunkSize, array, chunkSizes.AsSpan(), out var chunkCount))
                throw new InvalidOperationException($"Can't find a valid split for a {array.Length} length array to chunk sizes {minChunkSize}..{maxChunkSize}");

            var chunks = new T[chunkCount][];
            var arrayIndex = 0;
            for (var chunk = 0; chunk < chunkCount; ++chunk)
            {
                var chunkSize = chunkSizes[chunk];
                chunks[chunk] = array[arrayIndex..(arrayIndex + chunkSize)];
                arrayIndex += chunkSize;
            }

            return chunks;
        }
        finally
        {
            ArrayPool<int>.Shared.Return(chunkSizes);
        }
    }

    private static bool TryFindSplit<T>(int minChunkSize, int maxChunkSize, T[] array, Span<int> chunkSizes,
        out int chunkCount)
    {
        for (var chunkSize = maxChunkSize; chunkSize >= minChunkSize; --chunkSize)
        {
            var (fullChunks, leftover) = Math.DivRem(array.Length, chunkSize);
            if (leftover == 0)
            {
                // No leftover means we can use the full chunk size every time
                chunkCount = fullChunks;
                chunkSizes[..chunkCount].Fill(chunkSize);
                return true;
            }

            // Start with the leftover at the front
            chunkCount = fullChunks + 1;
            chunkSizes[0] = leftover;
            chunkSizes[1..chunkCount].Fill(chunkSize);
            if (leftover >= minChunkSize) return true;

            // Walk through the list borrowing enough from the next item to make each item
            // up to min size if it isn't yet. If I reach the end and I still have something
            // left to borrow, I've failed to generate a good list.
            // This function will also reverse the chunk size list, but that doesn't matter
            int debt = 0, index = 0;
            do
            {
                var withoutDebt = chunkSizes[index] - debt;
                if (withoutDebt >= minChunkSize)
                {
                    chunkSizes[index++] = withoutDebt;
                    debt = 0;
                }
                else
                {
                    chunkSizes[index++] = minChunkSize;
                    debt = minChunkSize - withoutDebt;
                }
            } while (index < chunkCount && debt > 0);

            if (debt == 0) return true;
        }

        // If we got here we failed to find a split
        chunkCount = 0;
        return false;
    }
}
