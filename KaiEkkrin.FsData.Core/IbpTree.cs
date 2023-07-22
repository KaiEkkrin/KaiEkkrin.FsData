using KaiEkkrin.FsData.Core.Immutable;
using System.Collections;

namespace KaiEkkrin.FsData.Core;

/// <summary>
/// An immutable B+ tree.
/// TODO also implement the dictionary interfaces, etc?
/// </summary>
public sealed class IbpTree<TKey, TValue> : IEnumerable<KeyValuePair<TKey, TValue>> where TKey : notnull
{
    private readonly int _b;
    private readonly int _count;
    private readonly int _depth;

    private readonly IComparer<TKey> _comparer;
    private readonly IEqualityComparer<TKey> _equalityComparer;
    private readonly Node<TKey, TValue> _node;

    internal IbpTree(int b, int count, int depth, IComparer<TKey> comparer, IEqualityComparer<TKey> equalityComparer,
        Node<TKey, TValue> node)
    {
        _b = b;
        _count = count;
        _depth = depth;
        _comparer = comparer;
        _equalityComparer = equalityComparer;
        _node = node;
    }

    internal void DebugValidate()
    {
        throw new NotImplementedException();
    }

    public IbpTree<TKey, TValue> Delete(TKey key)
    {
        throw new NotImplementedException();
    }

    public IEnumerable<KeyValuePair<TKey, TValue>> EnumerateFrom(TKey key)
    {
        throw new NotImplementedException();
    }

    public KeyValuePair<TKey, TValue> First()
    {
        throw new NotImplementedException();
    }

    public IbpTree<TKey, TValue> Insert(TKey key, TValue value)
    {
        throw new NotImplementedException();
    }

    public bool TryFind(TKey key, out TValue value)
    {
        throw new NotImplementedException();
    }

    public override string ToString()
    {
        // TODO debug print here
        throw new NotImplementedException();
    }

    #region IEnumerable

    public IEnumerator<KeyValuePair<TKey, TValue>> GetEnumerator()
    {
        throw new NotImplementedException();
    }

    IEnumerator IEnumerable.GetEnumerator()
    {
        throw new NotImplementedException();
    }

    #endregion
}

public static class IbpTree
{
    public static IbpTree<TKey, TValue> CreateFrom<TKey, TValue>(
        IEnumerable<KeyValuePair<TKey, TValue>> values, int b,
        IComparer<TKey>? comparer = null, IEqualityComparer<TKey>? equalityComparer = null)
        where TKey : notnull
    {
        comparer ??= Comparer<TKey>.Default;
        equalityComparer ??= EqualityComparer<TKey>.Default;

        // Suspicion section: make sure that the values are sorted in order of the
        // given comparer, and remove any with duplicate keys
        var valuesArray = ArrayUtil.SortedAndDistinct(comparer, equalityComparer, values);

        // TODO
        throw new NotImplementedException();
    }

}
