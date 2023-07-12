module IbpTree2Tests

open System
open Xunit
open Xunit.Abstractions

// Here I put any interesting specific test data I spot from the property tests and want to retry
type Tests(output: ITestOutputHelper) =

    [<Fact>]
    let ``This set of values inserted into a tree can be retrieved``() =
        let keys = [|15; 18; 12; 0; 6; 21; 24; 3; 9|]
        IbpTree2TestCommon.testInsertAndFind output (3, keys)

    [<Fact>]
    let ``This set of values created as a tree can be retrieved 1``() =
        let keys = [|0; 3; 6; 9; 12; 15; 18; 21; 24; 27; 30; 33; 36; 39; 42; 45; 48; 51|]

        // Debug out what this tree *should* end up looking like:
        output.WriteLine "Tree made with insert:"
        IbpTree2TestCommon.buildTreeWithDebug output (5, keys) |> ignore

        // Try to create it with the sequence
        output.WriteLine ""
        output.WriteLine "Tree made with createFrom:"
        IbpTree2TestCommon.testCreateAndFind output (5, keys)

    [<Fact>]
    let ``This set of values created as a tree can be retrieved 2``() =
        let keys = [|0; 3; 6; 9; 12; 15; 18; 21; 24; 27; 30; 33; 36; 39|]

        // Debug out what this tree *should* end up looking like:
        output.WriteLine "Tree made with insert:"
        IbpTree2TestCommon.buildTreeWithDebug output (3, keys) |> ignore

        // Try to create it with the sequence
        output.WriteLine ""
        output.WriteLine "Tree made with createFrom:"
        IbpTree2TestCommon.testCreateAndFind output (3, keys)

    [<Fact>]
    let ``This set of values created as a tree can be retrieved 3``() =
        let keys = [|9; 18; 6; 12; 15; 3; 0; 21; 0; 3; 6; 6; 15; 15; 15; 9; 21; 21; 18; 12; 3|]

        // Debug out what this tree *should* end up looking like:
        output.WriteLine "Tree made with insert:"
        IbpTree2TestCommon.buildTreeWithDebug output (3, keys) |> ignore

        // Try to create it with the sequence
        output.WriteLine ""
        output.WriteLine "Tree made with createFrom:"
        IbpTree2TestCommon.testCreateAndFind output (3, keys)

    [<Fact>]
    let ``This set of values in a tree can be deleted 1``() =
        let keys = [|15; 33; 3; 30; 0; 18; 6; 9; 36; 12; 27; 21; 24|]
        let deleteKeys = [|15; 3; 33; 30|]

        let tree =
            IbpTree2TestCommon.buildTreeWithDebug output (3, keys)
            |> IbpTree2TestCommon.deleteFromTreeWithDebug output deleteKeys

        output.WriteLine ""
        output.WriteLine <| sprintf "Tree after deleting %A:" deleteKeys
        output.WriteLine <| tree.ToString ()

        let notDeletedKeys = keys |> Array.except deleteKeys
        IbpTree2TestCommon.testFind output notDeletedKeys tree
        IbpTree2TestCommon.testNotFound deleteKeys tree

    [<Fact>]
    let ``This set of values in a tree can be deleted 2``() =
        let keys = [|3; 0|]
        let deleteKeys = [|0; 3|]

        let tree =
            IbpTree2TestCommon.createTreeWithDebug output (4, keys)
            |> IbpTree2TestCommon.deleteFromTreeWithDebug output deleteKeys

        output.WriteLine ""
        output.WriteLine <| sprintf "Tree after deleting %A:" deleteKeys
        output.WriteLine <| tree.ToString ()

        let notDeletedKeys = keys |> Array.except deleteKeys
        IbpTree2TestCommon.testFind output notDeletedKeys tree
        IbpTree2TestCommon.testNotFound deleteKeys tree

