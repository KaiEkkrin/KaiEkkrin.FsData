namespace KaiEkkrin.FsData.Data

open System
open System.Collections.Generic

// Trivial stuff used by the rest
module Common =

    let divCeil (a: int) b =
        let (div, rem) = Math.DivRem(a, b)
        if rem = 0 then div else div + 1

    type IndexedKeyValuePair<'TKey, 'TValue> = struct
        val Index: int
        val Key: 'TKey
        val Value: 'TValue
        new (index, kv: KeyValuePair<'TKey, 'TValue>) = { Index = index; Key = kv.Key; Value = kv.Value }
        end
    with
        member this.KeyValue = KeyValuePair(this.Key, this.Value)

    // Compares IndexedKeyValuePair<'TKey, 'TValue> by key (ascending), then index (descending.)
    type IndexedKeyValuePairKeyComparer<'TKey, 'TValue>(Comparer: IComparer<'TKey>) =
        interface IComparer< IndexedKeyValuePair<'TKey, 'TValue> > with
            member this.Compare (
                a: IndexedKeyValuePair<'TKey, 'TValue>,
                b: IndexedKeyValuePair<'TKey, 'TValue>
            ) =
                match Comparer.Compare (a.Key, b.Key) with
                | 0 -> -(a.Index.CompareTo b.Index)
                | n -> n

