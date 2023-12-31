module IbpTree2TestCommon

open System
open System.Collections.Generic
open System.Text
open FsUnit.Xunit
open KaiEkkrin.FsData.Data
open Xunit.Abstractions

let buildTreeWithDebug (output: ITestOutputHelper) (b, keys) =
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
    tree

let createTreeWithDebug (output: ITestOutputHelper) (b, keys) =
    let keyValues = keys |> Seq.map (fun k -> KeyValuePair(k, sprintf "%d" k))
    let tree = IbpTree2.createFromB b Comparer<int>.Default EqualityComparer<int>.Default keyValues
    output.WriteLine <| tree.ToString ()

    match IbpTree2.debugValidate tree with
    | Some err -> failwithf "When creating tree from sequence: %s" err
    | None -> tree

let deleteFromTreeWithDebug (output: ITestOutputHelper) keys tree =
    let updatedTree =
        keys
        |> Array.fold (fun t key ->
            // Run the validation on each step, to catch any invalid trees:
            let t2 = IbpTree2.delete key t
            match IbpTree2.debugValidate t2 with
            | None -> t2
            | Some err ->
                let sb = StringBuilder ()
                sprintf "When deleting %A: %s" key err |> sb.AppendLine |> ignore
                sb.AppendLine "-- Tree before --" |> ignore
                t.ToString() |> sb.AppendLine |> ignore
                sb.AppendLine "-- Tree after --" |> ignore
                t2.ToString() |> sb.AppendLine |> ignore
                failwith <| sb.ToString ()
        ) tree

    output.WriteLine <| tree.ToString ()
    updatedTree

let editTreeWithDebug (output: ITestOutputHelper) keys tree =
    let updatedTree =
        keys
        |> Array.fold (fun t key ->
            // Again validate on every step
            let t2 = IbpTree2.insert key (sprintf "E%d" key) t
            match IbpTree2.debugValidate t2 with
            | None -> t2
            | Some err ->
                let sb = StringBuilder ()
                sprintf "When editing %A: %s" key err |> sb.AppendLine |> ignore
                sb.AppendLine "-- Tree before --" |> ignore
                t.ToString() |> sb.AppendLine |> ignore
                sb.AppendLine "-- Tree after --" |> ignore
                t2.ToString() |> sb.AppendLine |> ignore
                failwith <| sb.ToString ()
        ) tree

    output.WriteLine <| tree.ToString ()
    updatedTree

let testFind (output: ITestOutputHelper) keys tree =
    // Check each distinct key is present
    let distinctKeys = keys |> Array.distinct
    for key in distinctKeys do
        let found = IbpTree2.tryFind key tree
        found |> should equal (Some (sprintf "%d" key))

        let notFound1 = IbpTree2.tryFind (key + 1) tree
        notFound1 |> should equal None

        let notFound2 = IbpTree2.tryFind (key - 1) tree
        notFound2 |> should equal None

    // Check that the forward enumeration of the whole tree equals the
    // ordered list of distinct key-value pairs
    let orderedKeys = distinctKeys |> Array.sort
    let orderedValues = orderedKeys |> Array.map (sprintf "%d")
    let maybeMinKey =
        distinctKeys
        |> Array.fold (fun st k -> match st with | Some v -> Some <| Math.Min (v, k) | None -> Some k) None

    match maybeMinKey with
    | Some minKey ->
        let enumeration = IbpTree2.enumerateFrom minKey tree |> Array.ofSeq

        let enumeratedKeys = enumeration |> Array.map (fun kv -> kv.Key)
        enumeratedKeys |> should equalSeq orderedKeys

        let enumeratedValues = enumeration |> Array.map (fun kv -> kv.Value)
        enumeratedValues |> should equalSeq orderedValues

    | None ->
        keys |> should be Empty

    // The implementation of IEnumerable<> should also work like that
    let e2 = tree |> Array.ofSeq

    let e2Keys = e2 |> Array.map (fun kv -> kv.Key)
    e2Keys |> should equalSeq orderedKeys

    let e2Values = e2 |> Array.map (fun kv -> kv.Value)
    e2Values |> should equalSeq orderedValues

let testFindEdited (output: ITestOutputHelper) keys editedKeys tree =
    // Check each distinct key is present
    let distinctKeys = keys |> Array.distinct
    for key in distinctKeys do
        let found = IbpTree2.tryFind key tree
        let expected =
            if (Array.contains key editedKeys) then (sprintf "E%d" key)
            else (sprintf "%d" key)

        found |> should equal (Some expected)

        let notFound1 = IbpTree2.tryFind (key + 1) tree
        notFound1 |> should equal None

        let notFound2 = IbpTree2.tryFind (key - 1) tree
        notFound2 |> should equal None

    // Check that the forward enumeration of the whole tree equals the
    // ordered list of distinct key-value pairs
    let orderedKeys = distinctKeys |> Array.sort
    let orderedValues = orderedKeys |> Array.map (fun key ->
        if (Array.contains key editedKeys) then (sprintf "E%d" key)
        else (sprintf "%d" key))

    let maybeMinKey =
        distinctKeys
        |> Array.fold (fun st k -> match st with | Some v -> Some <| Math.Min (v, k) | None -> Some k) None

    match maybeMinKey with
    | Some minKey ->
        let enumeration = IbpTree2.enumerateFrom minKey tree |> Array.ofSeq

        let enumeratedKeys = enumeration |> Array.map (fun kv -> kv.Key)
        enumeratedKeys |> should equalSeq orderedKeys

        let enumeratedValues = enumeration |> Array.map (fun kv -> kv.Value)
        enumeratedValues |> should equalSeq orderedValues

    | None ->
        keys |> should be Empty

    // The implementation of IEnumerable<> should also work like that
    let e2 = tree |> Array.ofSeq

    let e2Keys = e2 |> Array.map (fun kv -> kv.Key)
    e2Keys |> should equalSeq orderedKeys

    let e2Values = e2 |> Array.map (fun kv -> kv.Value)
    e2Values |> should equalSeq orderedValues

let testInsertAndFind (output: ITestOutputHelper) (b, keys) =
    let tree = buildTreeWithDebug output (b, keys)
    testFind output keys tree

let testCreateAndFind (output: ITestOutputHelper) (b, keys) =
    let tree = createTreeWithDebug output (b, keys)
    testFind output keys tree

let testNotFound keys tree =
    for key in keys do
        let found = IbpTree2.tryFind key tree
        found |> should equal None

