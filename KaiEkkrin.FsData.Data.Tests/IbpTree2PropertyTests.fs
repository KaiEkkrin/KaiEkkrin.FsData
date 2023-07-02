module IbpTree2PropertyTests

open FsCheck
open FsCheck.Xunit
open FsUnit.Xunit
open KaiEkkrin.FsData.Data
open Xunit.Abstractions

// TODO test for balancing (very important) and for the tree being validly organised
type PropertyTests(output: ITestOutputHelper) =

    // Generates a valid order for the tree (the B-value.)
    // Whilst all values >= 3 are valid, not all are equally interesting and I want to bias the test set
    // towards small and particular numbers. If these work, probably the rest should work...
    // TODO make bigger once things look like they're working
    //let genOrder = Gen.elements [3; 4; 5; 6; 7; 8; 13; 21; 34; 64; 99]
    let genOrder = Gen.elements [3; 4; 5]

    [<Property>]
    let ``An empty tree contains nothing``() =
        let arb = Arb.fromGen <| gen {
            let! b = genOrder
            let! key = Gen.choose (0, 1000)
            return (b, key)
        }

        Prop.forAll arb <| fun (b, key) ->
            let emptyTree = IbpTree2.emptyB<int, string>(b)
            let x = IbpTree2.tryFind key emptyTree
            x |> should equal None

    [<Property>]
    let ``A one-item tree contains one item``() =
        let arb = Arb.fromGen <| gen {
            let! b = genOrder
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

    [<Property>]
    let ``Values inserted randomly into a tree can be retrieved``() =
        let arb = Arb.fromGen <| gen {
            let! b = genOrder

            // Generate some keys to insert -- these will always be multiples of 3
            // so we can always look for a missing key in between valid keys
            // TODO Also test with duplicates!
            let! keys =
                Gen.choose (1, b * 4) // TODO make bigger once things look like they're working
                |> Gen.map (fun count -> Array.init count (fun i -> i * 3))

            let! shuffledKeys = Gen.shuffle keys
            return (b, shuffledKeys)
        }

        Prop.forAll arb <| IbpTree2TestCommon.testInsertAndFind output
