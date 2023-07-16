module ArrayUtilTests

open System
open System.Collections.Generic
open Xunit
open Xunit.Abstractions
open FsCheck
open FsCheck.Xunit
open FsUnit.Xunit
open KaiEkkrin.FsData.Data

type Tests(output: ITestOutputHelper) =

    // ## arraySplice1 tests ##

    [<Property>]
    let ``An empty array receives one item`` (item: int) =
        let spliced = ArrayUtil.arraySplice1 0 0 item [||]
        spliced |> should equal [|item|]

    [<Property>]
    let ``A value is inserted at the start``() =
        Prop.forAll (Arb.Default.PositiveInt()) <| fun l ->
            let length = l.Get
            let sourceArray = Array.init length (fun i -> sprintf "%d" i)
            let destinationArray = ArrayUtil.arraySplice1 0 0 "A" sourceArray
            Assert.Equal (length + 1, destinationArray.Length)
            destinationArray |> Array.iteri (fun i v ->
                if i = 0 then Assert.Equal ("A", v)
                else Assert.Equal (sprintf "%d" (i - 1), v))

    [<Property>]
    let ``A value is inserted at the end``() =
        Prop.forAll (Arb.Default.PositiveInt()) <| fun l ->
            let length = l.Get
            let sourceArray = Array.init length (fun i -> sprintf "%d" i)
            let destinationArray = ArrayUtil.arraySplice1 length 0 "A" sourceArray
            Assert.Equal (length + 1, destinationArray.Length)
            destinationArray |> Array.iteri (fun i v ->
                if i = length then Assert.Equal ("A", v)
                else Assert.Equal (sprintf "%d" i, v))

    [<Property>]
    let ``A value is inserted in the middle``() =
        // Generate a length and index such that
        // - length is >= 3
        // - index is > 0 and < length - 1
        let arb = Arb.fromGen <| gen {
            let! length = Gen.choose (3, 100)
            let! index = Gen.choose (1, length - 2)
            return (index, length)
        }

        Prop.forAll arb <| fun (index, length) ->
            let sourceArray = Array.init length (fun i -> sprintf "%d" i)
            let destinationArray = ArrayUtil.arraySplice1 index 0 "A" sourceArray
            Assert.Equal (length + 1, destinationArray.Length)
            destinationArray |> Array.iteri (fun i v ->
                if i = index then Assert.Equal ("A", v)
                else if i < index then Assert.Equal (sprintf "%d" i, v)
                else Assert.Equal (sprintf "%d" (i - 1), v))

    [<Property>]
    let ``A value is replaced at the start``() =
        Prop.forAll (Arb.Default.PositiveInt()) <| fun l ->
            let length = l.Get
            let sourceArray = Array.init length (fun i -> sprintf "%d" i)
            let destinationArray = ArrayUtil.arraySplice1 0 1 "A" sourceArray
            Assert.Equal (length, destinationArray.Length)
            destinationArray |> Array.iteri (fun i v ->
                if i = 0 then Assert.Equal ("A", v)
                else Assert.Equal (sprintf "%d" i, v))

    [<Property>]
    let ``A value is replaced at the end``() =
        Prop.forAll (Arb.Default.PositiveInt()) <| fun l ->
            let length = l.Get
            let sourceArray = Array.init length (fun i -> sprintf "%d" i)
            let destinationArray = ArrayUtil.arraySplice1 (length - 1) 1 "A" sourceArray
            Assert.Equal (length, destinationArray.Length)
            destinationArray |> Array.iteri (fun i v ->
                if i = (length - 1) then Assert.Equal ("A", v)
                else Assert.Equal (sprintf "%d" i, v))

    [<Property>]
    let ``A value is replaced in the middle``() =
        // Generate a length and index such that
        // - length is >= 3
        // - index is > 0 and < length - 1
        let arb = Arb.fromGen <| gen {
            let! length = Gen.choose (3, 100)
            let! index = Gen.choose (1, length - 2)
            return (index, length)
        }

        Prop.forAll arb <| fun (index, length) ->
            let sourceArray = Array.init length (fun i -> sprintf "%d" i)
            let destinationArray = ArrayUtil.arraySplice1 index 1 "A" sourceArray
            Assert.Equal (length, destinationArray.Length)
            destinationArray |> Array.iteri (fun i v ->
                if i = index then Assert.Equal ("A", v)
                else Assert.Equal (sprintf "%d" i, v))

    [<Property>]
    let ``The whole array is replaced``() =
        let arb = Gen.choose (1, 100) |> Arb.fromGen
        Prop.forAll arb <| fun length ->
            let sourceArray = Array.init length (fun i -> sprintf "%d" i)
            let destinationArray = ArrayUtil.arraySplice1 0 length "A" sourceArray
            Assert.Equal (1, destinationArray.Length)
            Assert.Equal ("A", destinationArray[0])

    // ## arraySpliceX tests (copies of the above) ##

    [<Property>]
    let ``An empty array receives one item`` (item: int) =
        let spliced = ArrayUtil.arraySpliceX 0 0 [|item|] [||]
        Assert.Equal (1, spliced.Length)
        Assert.Equal (item, spliced[0])

    [<Property>]
    let ``A value is inserted at the start``() =
        Prop.forAll (Arb.Default.PositiveInt()) <| fun l ->
            let length = l.Get
            let sourceArray = Array.init length (fun i -> sprintf "%d" i)
            let destinationArray = ArrayUtil.arraySpliceX 0 0 [|"A"|] sourceArray
            Assert.Equal (length + 1, destinationArray.Length)
            destinationArray |> Array.iteri (fun i v ->
                if i = 0 then Assert.Equal ("A", v)
                else Assert.Equal (sprintf "%d" (i - 1), v))

    [<Property>]
    let ``A value is inserted at the end``() =
        Prop.forAll (Arb.Default.PositiveInt()) <| fun l ->
            let length = l.Get
            let sourceArray = Array.init length (fun i -> sprintf "%d" i)
            let destinationArray = ArrayUtil.arraySpliceX length 0 [|"A"|] sourceArray
            Assert.Equal (length + 1, destinationArray.Length)
            destinationArray |> Array.iteri (fun i v ->
                if i = length then Assert.Equal ("A", v)
                else Assert.Equal (sprintf "%d" i, v))

    [<Property>]
    let ``A value is inserted in the middle``() =
        // Generate a length and index such that
        // - length is >= 3
        // - index is > 0 and < length - 1
        let arb = Arb.fromGen <| gen {
            let! length = Gen.choose (3, 100)
            let! index = Gen.choose (1, length - 2)
            return (index, length)
        }

        Prop.forAll arb <| fun (index, length) ->
            let sourceArray = Array.init length (fun i -> sprintf "%d" i)
            let destinationArray = ArrayUtil.arraySpliceX index 0 [|"A"|] sourceArray
            Assert.Equal (length + 1, destinationArray.Length)
            destinationArray |> Array.iteri (fun i v ->
                if i = index then Assert.Equal ("A", v)
                else if i < index then Assert.Equal (sprintf "%d" i, v)
                else Assert.Equal (sprintf "%d" (i - 1), v))

    [<Property>]
    let ``A value is replaced at the start``() =
        Prop.forAll (Arb.Default.PositiveInt()) <| fun l ->
            let length = l.Get
            let sourceArray = Array.init length (fun i -> sprintf "%d" i)
            let destinationArray = ArrayUtil.arraySpliceX 0 1 [|"A"|] sourceArray
            Assert.Equal (length, destinationArray.Length)
            destinationArray |> Array.iteri (fun i v ->
                if i = 0 then Assert.Equal ("A", v)
                else Assert.Equal (sprintf "%d" i, v))

    [<Property>]
    let ``A value is replaced at the end``() =
        Prop.forAll (Arb.Default.PositiveInt()) <| fun l ->
            let length = l.Get
            let sourceArray = Array.init length (fun i -> sprintf "%d" i)
            let destinationArray = ArrayUtil.arraySpliceX (length - 1) 1 [|"A"|] sourceArray
            Assert.Equal (length, destinationArray.Length)
            destinationArray |> Array.iteri (fun i v ->
                if i = (length - 1) then Assert.Equal ("A", v)
                else Assert.Equal (sprintf "%d" i, v))

    [<Property>]
    let ``A value is replaced in the middle``() =
        // Generate a length and index such that
        // - length is >= 3
        // - index is > 0 and < length - 1
        let arb = Arb.fromGen <| gen {
            let! length = Gen.choose (3, 100)
            let! index = Gen.choose (1, length - 2)
            return (index, length)
        }

        Prop.forAll arb <| fun (index, length) ->
            let sourceArray = Array.init length (fun i -> sprintf "%d" i)
            let destinationArray = ArrayUtil.arraySpliceX index 1 [|"A"|] sourceArray
            Assert.Equal (length, destinationArray.Length)
            destinationArray |> Array.iteri (fun i v ->
                if i = index then Assert.Equal ("A", v)
                else Assert.Equal (sprintf "%d" i, v))

    [<Property>]
    let ``The whole array is replaced``() =
        let arb = Gen.choose (1, 100) |> Arb.fromGen
        Prop.forAll arb <| fun length ->
            let sourceArray = Array.init length (fun i -> sprintf "%d" i)
            let destinationArray = ArrayUtil.arraySpliceX 0 length [|"A"|] sourceArray
            Assert.Equal (1, destinationArray.Length)
            Assert.Equal ("A", destinationArray[0])

    // ## arraySpliceX tests (multiple values) -- just a scattering of these, should be OK //
    [<Property>]
    let ``An empty array receives multiple items``() =
        let arb = Gen.choose (1, 100) |> Arb.fromGen
        Prop.forAll arb <| fun length ->
            let newItems = Array.init length (fun i -> sprintf "%d" i)
            let spliced = ArrayUtil.arraySpliceX 0 0 newItems [||]           
            Assert.Equal (length, spliced.Length)
            spliced |> Array.iteri (fun i v ->
                Assert.Equal (sprintf "%d" i, v))

    [<Property>]
    let ``Splice all items except the first and the last``() =
        let arb = Arb.fromGen <| gen {
            let! sourceLength = Gen.choose (3, 100)
            let! spliceLength = Gen.choose (1, 100)
            return (sourceLength, spliceLength)
        }
        
        Prop.forAll arb <| fun (sourceLength, spliceLength) ->
            let sourceArray = Array.init sourceLength (fun i -> sprintf "%d" i)
            let spliceArray = Array.init spliceLength (fun i -> sprintf "X%d" i)
            let destinationArray = ArrayUtil.arraySpliceX 1 (sourceLength - 2) spliceArray sourceArray

            Assert.Equal (spliceLength + 2, destinationArray.Length)
            Assert.Equal ("0", destinationArray[0])
            Assert.Equal (sprintf "%d" (sourceLength - 1), destinationArray[destinationArray.Length - 1])
            destinationArray |> Seq.skip 1 |> Seq.take spliceLength |> Seq.iteri (fun i v ->
                Assert.Equal (sprintf "X%d" i, v))

    // The splitIntoChunks function may not always be able to give sensible answers, but I
    // only need it to be correct for minChunkSize..maxChunkSize that could be
    // used within the tree:
    let genChunkSizeRange = gen {
        let! b = TestCommon.genBValue
        let minChunkSize = Math.Min (IbpTree2.getLengthOfSplitIntNode b, IbpTree2.getLengthOfSplitLeafNode b)
        let maxChunkSize = b - 1
        return (minChunkSize, maxChunkSize)
    }

    [<Property>]
    let ``SplitIntoChunks always returns a short array as a single chunk``() =
        let arb = Arb.fromGen <| gen {
            let! (minChunkSize, maxChunkSize) = genChunkSizeRange
            let! arrayLength = Gen.choose (0, maxChunkSize)
            let! array = Gen.arrayOfLength arrayLength (Gen.choose (0, 9_999))
            return (minChunkSize, maxChunkSize, array)
        }

        Prop.forAll arb <| fun (minChunkSize, maxChunkSize, array) ->
            let chunks = ArrayUtil.splitIntoChunks minChunkSize maxChunkSize array

            chunks.Length |> should equal 1
            chunks[0] |> should equalSeq array

    [<Property>]
    let ``SplitIntoChunks always splits long arrays into valid chunks``() =
        let arb = Arb.fromGen <| gen {
            let! (minChunkSize, maxChunkSize) = genChunkSizeRange
            let! arrayLength = Gen.choose (maxChunkSize + 1, maxChunkSize * 100)
            let! array = Gen.arrayOfLength arrayLength (Gen.choose (0, 9_999))
            return (minChunkSize, maxChunkSize, array)
        }

        Prop.forAll arb <| fun (minChunkSize, maxChunkSize, array) ->
            let chunks = ArrayUtil.splitIntoChunks minChunkSize maxChunkSize array

            // Every chunk must be between min and max chunk sizes (inclusive)
            for chunk in chunks do
                chunk.Length |> should greaterThanOrEqualTo minChunkSize 
                chunk.Length |> should lessThanOrEqualTo maxChunkSize

            // The collected array should be the same as the original
            let collected = chunks |> Array.collect id
            collected |> should equalSeq array

    let genDistinctKeyValues = gen {
        let! arrayLength = Gen.choose (0, 1000)
        let keysInOrder = Array.init arrayLength id
        let! keys = Gen.shuffle keysInOrder
        return keys |> Array.map (fun i -> KeyValuePair(i, sprintf "%d" i))
    }

    [<Property>]
    let ``SortedAndDistinct always returns a sorted array``() =
        let arb = Arb.fromGen <| genDistinctKeyValues
        Prop.forAll arb <| fun array ->
            let expected = array |> Array.sortBy (fun kv -> kv.Key)
            let actual = ArrayUtil.sortedAndDistinct Comparer<Int32>.Default EqualityComparer<Int32>.Default array
            actual |> should equalSeq expected

    [<Property>]
    let ``SortedAndDistinct always excludes older items with concatenated arrays``() =
        // In this one, I'll generate three arrays of the same sequence of shuffled
        // keys (with different values) and expect only the last series of values to
        // come out.
        let arb = Arb.fromGen <| genDistinctKeyValues
        Prop.forAll arb <| fun array ->
            let partA = array |> Array.map (fun kv -> KeyValuePair(kv.Key, sprintf "A%s" kv.Value))
            let partB = array |> Array.map (fun kv -> KeyValuePair(kv.Key, sprintf "B%s" kv.Value))
            let partC = array |> Array.map (fun kv -> KeyValuePair(kv.Key, sprintf "C%s" kv.Value))
            let allParts = Array.concat [|partA; partB; partC|]

            let expected = partC |> Array.sortBy (fun kv -> kv.Key)
            let actual = ArrayUtil.sortedAndDistinct Comparer<Int32>.Default EqualityComparer<Int32>.Default allParts
            actual |> should equalSeq expected

    [<Property>]
    let ``SortedAndDistinct always excludes older items with interleaved arrays``() =
        // In this one, I'll duplicate each key three times (with different values) and
        // expect only the final keys to come out.
        let arb = Arb.fromGen <| genDistinctKeyValues
        Prop.forAll arb <| fun array ->
            let withDuplicates = [|
                for kv in array do
                    yield KeyValuePair (kv.Key, sprintf "A%s" kv.Value)
                    yield KeyValuePair (kv.Key, sprintf "B%s" kv.Value)
                    yield KeyValuePair (kv.Key, sprintf "C%s" kv.Value)
            |]

            let expected =
                array
                |> Array.map (fun kv -> KeyValuePair(kv.Key, sprintf "C%s" kv.Value))
                |> Array.sortBy (fun kv -> kv.Key)

            let actual = ArrayUtil.sortedAndDistinct Comparer<Int32>.Default EqualityComparer<Int32>.Default withDuplicates
            actual |> should equalSeq expected

