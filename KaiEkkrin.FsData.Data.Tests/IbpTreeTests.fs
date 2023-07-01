module IbpTreeTests

open FsCheck.Xunit
open KaiEkkrin.FsData.Data
open Xunit
open Xunit.Abstractions

type Tests(output: ITestOutputHelper) =
    [<Property>]
    let ``An empty tree contains nothing`` (key: int) =
        let emptyTree = IbpTree.emptyB<int, string> 4
        let x = IbpTree.tryFind key emptyTree
        Assert.Equal (None, x)

    [<Property>]
    let ``A single value inserted into a tree can be retrieved`` (kv: int * string) =
        let (key, value) = kv
        let tree =
            IbpTree.emptyB<int, string> 4
            |> IbpTree.insert key value

        match IbpTree.tryFind key tree with
        | Some found -> do
            Assert.Equal (key, found.Key)
            Assert.Equal (value, found.Value)
        | None ->
            Assert.True (false, $"Key {key} not found")

    [<Property>]
    let ``Values inserted into a tree can be retrieved`` (kvs: (int * string) list) =
        let tree =
            kvs
            |> List.fold (fun t (k, v) -> IbpTree.insert k v t) (IbpTree.emptyB<int, string> 4)

        let expected =
            kvs
            |> List.fold (fun m (k, v) -> Map.add k v m) Map.empty

        output.WriteLine <| sprintf "Tree for %A: %A" kvs tree

        for (k, v) in expected |> Map.toSeq do
            match IbpTree.tryFind k tree with
            | Some found -> do
                Assert.Equal (k, found.Key)
                Assert.Equal (v, found.Value)
            | None ->
                Assert.True (false, $"Key {k} not found") // it should be found!
