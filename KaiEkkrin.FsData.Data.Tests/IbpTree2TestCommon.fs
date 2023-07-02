module IbpTree2TestCommon

open FsUnit.Xunit
open KaiEkkrin.FsData.Data
open Xunit.Abstractions

let testInsertAndFind (output: ITestOutputHelper) (b, keys) =
    let emptyTree = IbpTree2.emptyB<int, string>(b)
    let tree =
        keys
        |> Array.fold (fun t key ->
            IbpTree2.insert key (sprintf "%d" key) t
        ) emptyTree

    output.WriteLine <| tree.ToString ()

    for key in keys do
        let found = IbpTree2.tryFind key tree
        found |> should equal (Some (sprintf "%d" key))

        let notFound1 = IbpTree2.tryFind (key + 1) tree
        notFound1 |> should equal None

        let notFound2 = IbpTree2.tryFind (key - 1) tree
        notFound2 |> should equal None
