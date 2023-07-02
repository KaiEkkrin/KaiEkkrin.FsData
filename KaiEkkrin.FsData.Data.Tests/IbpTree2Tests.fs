module IbpTree2Tests

open Xunit
open Xunit.Abstractions

type Tests(output: ITestOutputHelper) =

    // This is an interesting test case from the property tests that I spotted and I want to keep it
    [<Fact>]
    let ``This set of values inserted into a tree can be retrieved``() =
        let keys = [|15; 18; 12; 0; 6; 21; 24; 3; 9|]
        IbpTree2TestCommon.testInsertAndFind output (3, keys)
