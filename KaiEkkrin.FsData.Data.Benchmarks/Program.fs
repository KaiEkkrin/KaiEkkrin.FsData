open System
open System.Collections.Generic
open System.Collections.Immutable
open BenchmarkDotNet.Attributes
open BenchmarkDotNet.Running
open KaiEkkrin.FsData.Data

// See https://phillipcarter.dev/posts/benchmarking-fsharp/ to get started with BenchmarkDotNet in F#
// Run with profiling using `dotnet run -c Release -- -p EP`
// Analyse .speedscope.json files with https://www.speedscope.app/
// (see https://benchmarkdotnet.org/articles/features/event-pipe-profiler.html for more benchmark details)

// TODO try using other, more complicated keys and values here.
// For now I'm going to use a string key and this value type:
type TestValue = {
    Num1: int
    Num2: int
    Num3: int
}

let random = new Random(598237459)
let (buf: byte[]) = Array.zeroCreate 16
let genRandomKeyValue _ =
    random.NextBytes buf
    let key = Convert.ToBase64String buf
    let value = { Num1 = random.Next(); Num2 = random.Next(); Num3 = random.Next() }
    KeyValuePair (key, value)

let randomKeyValues = Array.init 1000 genRandomKeyValue
let lotsMoreRandomKeyValues = Array.init 100_000 genRandomKeyValue

let randomKeysToDelete = Array.init 1000 <| fun i ->
    let index = random.Next (0, 99_999)
    lotsMoreRandomKeyValues[index].Key
    
let largeTree3 = IbpTree2.createFromB 3 StringComparer.Ordinal StringComparer.Ordinal lotsMoreRandomKeyValues
let largeTree16 = IbpTree2.createFromB 16 StringComparer.Ordinal StringComparer.Ordinal lotsMoreRandomKeyValues
let largeTree128 = IbpTree2.createFromB 128 StringComparer.Ordinal StringComparer.Ordinal lotsMoreRandomKeyValues

let largeIsd =
    let builder = ImmutableSortedDictionary<string, TestValue>.Empty.WithComparers(StringComparer.Ordinal).ToBuilder()
    for kv in lotsMoreRandomKeyValues do
        builder[kv.Key] <- kv.Value

    builder.ToImmutable()

[<MemoryDiagnoser>]
type TreeCreate() =
    [<Benchmark(Description = "Create IbpTree2(3) from 1000 random values")>]
    member this.CreateIbp3() =
        IbpTree2.createFromB 3 StringComparer.Ordinal StringComparer.Ordinal randomKeyValues

    [<Benchmark(Description = "Create IbpTree2(16) from 1000 random values")>]
    member this.CreateIbp16() =
        IbpTree2.createFromB 16 StringComparer.Ordinal StringComparer.Ordinal randomKeyValues

    [<Benchmark(Description = "Create IbpTree2(128) from 1000 random values")>]
    member this.CreateIbp128() =
        IbpTree2.createFromB 128 StringComparer.Ordinal StringComparer.Ordinal randomKeyValues

    [<Benchmark(Description = "Create ImmutableSortedDictionary from 1000 random values", Baseline = true)>]
    member this.CreateIsd() =
        let builder = ImmutableSortedDictionary<string, TestValue>.Empty.WithComparers(StringComparer.Ordinal).ToBuilder()
        for kv in randomKeyValues do
            builder[kv.Key] <- kv.Value

        builder.ToImmutable()

[<MemoryDiagnoser>]
type TreeInsertEmpty() =
    [<Benchmark(Description = "Insert 1000 random values into empty IbpTree2(3)")>]
    member this.InsertEmptyIbp3() =
        let emptyTree = IbpTree2.createB 3 StringComparer.Ordinal
        randomKeyValues |> Array.fold (fun t kv -> IbpTree2.insert kv.Key kv.Value t) emptyTree

    [<Benchmark(Description = "Insert 1000 random values into empty IbpTree2(16)")>]
    member this.InsertEmptyIbp16() =
        let emptyTree = IbpTree2.createB 16 StringComparer.Ordinal
        randomKeyValues |> Array.fold (fun t kv -> IbpTree2.insert kv.Key kv.Value t) emptyTree

    [<Benchmark(Description = "Insert 1000 random values into empty IbpTree2(128)")>]
    member this.InsertEmptyIbp128() =
        let emptyTree = IbpTree2.createB 128 StringComparer.Ordinal
        randomKeyValues |> Array.fold (fun t kv -> IbpTree2.insert kv.Key kv.Value t) emptyTree

    [<Benchmark(Description = "Insert 1000 random values into empty ImmutableSortedDictionary", Baseline = true)>]
    member this.InsertEmptyIsd() =
        let emptyTree = ImmutableSortedDictionary<string, TestValue>.Empty.WithComparers(StringComparer.Ordinal)
        randomKeyValues |> Array.fold (fun (t: ImmutableSortedDictionary<string, TestValue>) kv -> t.SetItem (kv.Key, kv.Value)) emptyTree

[<MemoryDiagnoser>]
type TreeInsertLarge() =
    [<Benchmark(Description = "Insert 1000 random values into large IbpTree2(3)")>]
    member this.InsertLargeIbp3() =
        randomKeyValues |> Array.fold (fun t kv -> IbpTree2.insert kv.Key kv.Value t) largeTree3

    [<Benchmark(Description = "Insert 1000 random values into large IbpTree2(16)")>]
    member this.InsertLargeIbp16() =
        randomKeyValues |> Array.fold (fun t kv -> IbpTree2.insert kv.Key kv.Value t) largeTree16

    [<Benchmark(Description = "Insert 1000 random values into large IbpTree2(128)")>]
    member this.InsertLargeIbp128() =
        randomKeyValues |> Array.fold (fun t kv -> IbpTree2.insert kv.Key kv.Value t) largeTree128

    [<Benchmark(Description = "Insert 1000 random values into large ImmutableSortedDictionary", Baseline = true)>]
    member this.InsertLargeIsd() =
        randomKeyValues |> Array.fold (fun (t: ImmutableSortedDictionary<string, TestValue>) kv -> t.SetItem (kv.Key, kv.Value)) largeIsd

[<MemoryDiagnoser>]
type TreeDelete() =
    [<Benchmark(Description = "Delete 1000 random values from large IbpTree2(3)")>]
    member this.DeleteLargeIbp3() =
        randomKeysToDelete |> Array.fold (fun t k -> IbpTree2.delete k t) largeTree3

    [<Benchmark(Description = "Delete 1000 random values from large IbpTree2(16)")>]
    member this.DeleteLargeIbp16() =
        randomKeysToDelete |> Array.fold (fun t k -> IbpTree2.delete k t) largeTree16

    [<Benchmark(Description = "Delete 1000 random values from large IbpTree2(128)")>]
    member this.DeleteLargeIbp128() =
        randomKeysToDelete |> Array.fold (fun t k -> IbpTree2.delete k t) largeTree128

    [<Benchmark(Description = "Delete 1000 random values from large ImmutableSortedDictionary", Baseline = true)>]
    member this.DeleteLargeIsd() =
        randomKeysToDelete |> Array.fold (fun (t: ImmutableSortedDictionary<string, TestValue>) k -> t.Remove k) largeIsd

[<MemoryDiagnoser>]
type TreeEnumerateFirst() =
    [<Benchmark(Description = "Enumerate the first 1000 values from large IbpTree2(3)")>]
    member this.EnumerateStartLargeIbp3() =
        largeTree3 |> Seq.take 1000 |> Seq.last

    [<Benchmark(Description = "Enumerate the first 1000 values from large IbpTree2(16)")>]
    member this.EnumerateStartLargeIbp16() =
        largeTree16 |> Seq.take 1000 |> Seq.last

    [<Benchmark(Description = "Enumerate the first 1000 values from large IbpTree2(128)")>]
    member this.EnumerateStartLargeIbp128() =
        largeTree128 |> Seq.take 1000 |> Seq.last

    [<Benchmark(Description = "Enumerate the first 1000 values from large ImmutableSortedDictionary", Baseline = true)>]
    member this.EnumerateStartLargeIsd() =
        largeIsd |> Seq.take 1000 |> Seq.last

[<MemoryDiagnoser>]
type TreeEnumerateMid() =
    [<Benchmark(Description = "Enumerate 1000 values from within a large IbpTree2(3)")>]
    member this.EnumerateMidLargeIbp3() =
        largeTree3 |> IbpTree2.enumerateFrom (randomKeysToDelete[0]) |> Seq.take 1000 |> Seq.last

    [<Benchmark(Description = "Enumerate 1000 values from within a large IbpTree2(16)")>]
    member this.EnumerateMidLargeIbp16() =
        largeTree16 |> IbpTree2.enumerateFrom (randomKeysToDelete[0]) |> Seq.take 1000 |> Seq.last

    [<Benchmark(Description = "Enumerate 1000 values from within a large IbpTree2(128)")>]
    member this.EnumerateMidLargeIbp128() =
        largeTree128 |> IbpTree2.enumerateFrom (randomKeysToDelete[0]) |> Seq.take 1000 |> Seq.last

    [<Benchmark(Description = "Enumerate 1000 values from within a large ImmutableSortedDictionary", Baseline = true)>]
    member this.EnumerateMidLargeIsd() =
        largeIsd |> Seq.filter (fun kv -> kv.Key < randomKeysToDelete[0]) |> Seq.take 1000 |> Seq.last

[<MemoryDiagnoser>]
type TreeFind() =
    // so we're not always looking for the same one
    static let mutable index3 = 0
    static let mutable index16 = 0
    static let mutable index128 = 0
    static let mutable indexIsd = 0

    [<Benchmark(Description = "Find a value from within a large IbpTree2(3)")>]
    member this.FindIbp3() =
        largeTree3 |> IbpTree2.tryFind randomKeysToDelete[index3] |> ignore
        index3 <- if index3 = 999 then 0 else index3 + 1

    [<Benchmark(Description = "Find a value from within a large IbpTree2(16)")>]
    member this.FindIbp16() =
        largeTree16 |> IbpTree2.tryFind randomKeysToDelete[index16] |> ignore
        index16 <- if index16 = 999 then 0 else index16 + 1

    [<Benchmark(Description = "Find a value from within a large IbpTree2(128)")>]
    member this.FindIbp128() =
        largeTree128 |> IbpTree2.tryFind randomKeysToDelete[index128] |> ignore
        index128 <- if index128 = 999 then 0 else index128 + 1

    [<Benchmark(Description = "Find a value from within a large ImmutableSortedDictionary", Baseline = true)>]
    member this.FindIsd() =
        largeIsd.TryGetValue randomKeysToDelete[indexIsd] |> ignore
        indexIsd <- if indexIsd = 999 then 0 else indexIsd + 1

[<EntryPoint>]
let main argv =
    BenchmarkSwitcher.FromAssembly(typeof<TreeCreate>.Assembly).Run argv |> ignore
    0

