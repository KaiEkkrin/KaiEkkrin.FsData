module IbpTree2PropertyTests

open FsCheck
open FsCheck.Xunit
open FsUnit.Xunit
open KaiEkkrin.FsData.Data
open Xunit.Abstractions

// TODO test for balancing (very important) and for the tree being validly organised
type PropertyTests(output: ITestOutputHelper) =

    [<Property>]
    let ``An empty tree contains nothing``() =
        let arb = Arb.fromGen <| gen {
            let! b = TestCommon.genBValue
            let! key = Gen.choose (0, 1000)
            return (b, key)
        }

        Prop.forAll arb <| fun (b, key) ->
            let emptyTree = IbpTree2.emptyB<int, string>(b)
            let x = IbpTree2.tryFind key emptyTree
            x |> should equal None

    [<Property(MaxTest = 500)>]
    let ``A one-item tree contains one item``() =
        let arb = Arb.fromGen <| gen {
            let! b = TestCommon.genBValue
            let! key = Gen.choose (0, 1000)
            let! nonMatchingKey =
                Gen.choose (0, 1000)
                |> Gen.map (fun i -> if i >= key then i + 1 else i)

            return (b, key, nonMatchingKey)
        }

        Prop.forAll arb <| fun (b, key, nonMatchingKey) ->
            let tree =
                IbpTree2.emptyB<int, string>(b)
                |> IbpTree2.insert key "A"

            let found = IbpTree2.tryFind key tree
            found |> should equal (Some "A")

            let notFound = IbpTree2.tryFind nonMatchingKey tree
            notFound |> should equal None

    [<Property(MaxTest = 500)>]
    let ``Values inserted randomly into a tree can be retrieved``() =
        let arb = Arb.fromGen <| gen {
            let! b = TestCommon.genBValue

            // Generate some keys to insert -- these will always be multiples of 3
            // so we can always look for a missing key in between valid keys
            let! keys =
                Gen.choose (1, b * 7)
                |> Gen.map (fun count -> Array.init count (fun i -> i * 3))

            let! shuffledKeys = Gen.shuffle keys
            return (b, shuffledKeys)
        }

        Prop.forAll arb <| IbpTree2TestCommon.testInsertAndFind output

    [<Property(MaxTest = 500)>]
    let ``Values inserted in order into a tree can be retrieved``() =
        let arb = Arb.fromGen <| gen {
            let! b = TestCommon.genBValue

            // Generate some keys to insert -- these will always be multiples of 3
            // so we can always look for a missing key in between valid keys
            let! keys =
                Gen.choose (1, b * 7)
                |> Gen.map (fun count -> Array.init count (fun i -> i * 3))

            return (b, keys)
        }

        Prop.forAll arb <| IbpTree2TestCommon.testInsertAndFind output

    [<Property(MaxTest = 500)>]
    let ``Values inserted in reverse order into a tree can be retrieved``() =
        let arb = Arb.fromGen <| gen {
            let! b = TestCommon.genBValue

            // Generate some keys to insert -- these will always be multiples of 3
            // so we can always look for a missing key in between valid keys
            let! keys =
                Gen.choose (1, b * 7)
                |> Gen.map (fun count -> Array.init count (fun i -> i * 3))

            return (b, keys |> Array.rev)
        }

        Prop.forAll arb <| IbpTree2TestCommon.testInsertAndFind output

    [<Property(MaxTest = 500)>]
    let ``Values including duplicates inserted randomly into a tree can be retrieved``() =
        let arb = Arb.fromGen <| gen {
            let! b = TestCommon.genBValue

            // Generate some keys to insert -- these will always be multiples of 3
            // so we can always look for a missing key in between valid keys
            let! keys =
                Gen.choose (1, b * 7)
                |> Gen.map (fun count -> Array.init count (fun i -> i * 3))

            let! shuffledKeys = Gen.shuffle keys
            let! duplicateKeys =
                Array.concat [|keys; keys; keys|]
                |> Gen.shuffle

            let withDuplicates = Array.concat [|shuffledKeys; duplicateKeys[..(b * 4)]|]
            return (b, withDuplicates)
        }

        Prop.forAll arb <| IbpTree2TestCommon.testInsertAndFind output

    [<Property(MaxTest = 500)>]
    let ``Forward enumeration works from any point``() =
        let arb = Arb.fromGen <| gen {
            let! b = TestCommon.genBValue

            // As above.
            let! keys =
                Gen.choose (1, b * 7)
                |> Gen.map (fun count -> Array.init count (fun i -> i * 3))

            // Enumerate from any number within the bounds of the sequence, or
            // one outside of it.
            let maxKey = Array.max keys
            let! enumStart = Gen.choose (-1, maxKey + 1)

            let! shuffledKeys = Gen.shuffle keys
            return (b, shuffledKeys, enumStart)
        }

        Prop.forAll arb <| fun (b, keys, enumStart) ->
            let tree = IbpTree2TestCommon.buildTreeWithDebug output (b, keys)

            let orderedKeys =
                keys
                |> Array.filter (fun k -> k >= enumStart)
                |> Array.distinct
                |> Array.sort

            let orderedValues = orderedKeys |> Array.map (sprintf "%d")
            let enumeration = IbpTree2.enumerateFrom enumStart tree |> Array.ofSeq

            let enumeratedKeys = enumeration |> Array.map (fun kv -> kv.Key)
            enumeratedKeys |> should equalSeq orderedKeys

            let enumeratedValues = enumeration |> Array.map (fun kv -> kv.Value)
            enumeratedValues |> should equalSeq orderedValues

    [<Property(MaxTest = 500)>]
    let ``Keys not in a tree have no effect when deleted``() =
        let arb = Arb.fromGen <| gen {
            let! b = TestCommon.genBValue

            // Generate some keys to insert -- these will always be multiples of 3
            // so we can always look for a missing key in between valid keys
            let! keys =
                Gen.choose (1, b * 7)
                |> Gen.map (fun count -> Array.init count (fun i -> i * 3))

            let! shuffledKeys = Gen.shuffle keys
            let! notPresentKey = Gen.choose (-1000, -1)
            return (b, shuffledKeys, notPresentKey)
        }

        Prop.forAll arb <| fun (b, keys, notPresentKey) ->
            let tree =
                IbpTree2TestCommon.buildTreeWithDebug output (b, keys)
                |> IbpTree2.delete notPresentKey

            IbpTree2TestCommon.testFind output keys tree

    [<Property(MaxTest = 500)>]
    let ``Keys in a tree can be deleted``() =
        let arb = Arb.fromGen <| gen {
            let! b = TestCommon.genBValue

            // Generate some keys to insert -- these will always be multiples of 3
            // so we can always look for a missing key in between valid keys
            let! keys =
                Gen.choose (1, b * 7)
                |> Gen.map (fun count -> Array.init count (fun i -> i * 3))

            let! shuffledKeys = Gen.shuffle keys

            let! deleteCount = Gen.choose (1, keys.Length)
            let! deleteKeys = shuffledKeys[..(deleteCount - 1)] |> Gen.shuffle
            return (b, shuffledKeys, deleteKeys)
        }

        Prop.forAll arb <| fun (b, keys, deleteKeys) ->
            if (deleteKeys.Length = 0) then failwith "no delete keys"

            let tree =
                IbpTree2TestCommon.buildTreeWithDebug output (b, keys)
                |> IbpTree2TestCommon.deleteFromTreeWithDebug output deleteKeys

            let notDeletedKeys = keys |> Array.except deleteKeys
            IbpTree2TestCommon.testFind output notDeletedKeys tree
            IbpTree2TestCommon.testNotFound deleteKeys tree

    [<Property(MaxTest = 500)>]
    let ``Values in a tree can be edited``() =
        let arb = Arb.fromGen <| gen {
            let! b = TestCommon.genBValue

            // Generate some keys to insert -- these will always be multiples of 3
            // so we can always look for a missing key in between valid keys
            let! keys =
                Gen.choose (1, b * 7)
                |> Gen.map (fun count -> Array.init count (fun i -> i * 3))

            let! shuffledKeys = Gen.shuffle keys

            let! editCount = Gen.choose (1, keys.Length)
            let! editKeys = shuffledKeys[..(editCount - 1)] |> Gen.shuffle
            return (b, shuffledKeys, editKeys)
        }

        Prop.forAll arb <| fun (b, keys, editKeys) ->
            if (editKeys.Length = 0) then failwith "no edit keys"

            let tree =
                IbpTree2TestCommon.buildTreeWithDebug output (b, keys)
                |> IbpTree2TestCommon.editTreeWithDebug output editKeys

            IbpTree2TestCommon.testFindEdited output keys editKeys tree

