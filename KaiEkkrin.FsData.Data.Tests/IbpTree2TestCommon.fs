module IbpTree2TestCommon

open System.Text
open FsUnit.Xunit
open KaiEkkrin.FsData.Data
open Xunit.Abstractions

let testInsertAndFind (output: ITestOutputHelper) (b, keys) =
    let emptyTree = IbpTree2.emptyB<int, string>(b)
    let tree =
        keys
        |> Array.fold (fun t key ->
            // Run the validation on each step, to catch any invalid trees:
            let t2 = IbpTree2.insert key (sprintf "%d" key) t
            match IbpTree2.debugValidate t2 with
            | None -> t2
            | Some err ->
                let sb = StringBuilder ()
                sprintf "When adding %A: %s" key err |> sb.AppendLine |> ignore
                sb.AppendLine "-- Tree before --" |> ignore
                t.ToString() |> sb.AppendLine |> ignore
                sb.AppendLine "-- Tree after --" |> ignore
                t2.ToString() |> sb.AppendLine |> ignore
                failwith <| sb.ToString ()
        ) emptyTree

    output.WriteLine <| tree.ToString ()

    for key in keys do
        let found = IbpTree2.tryFind key tree
        found |> should equal (Some (sprintf "%d" key))

        let notFound1 = IbpTree2.tryFind (key + 1) tree
        notFound1 |> should equal None

        let notFound2 = IbpTree2.tryFind (key - 1) tree
        notFound2 |> should equal None
