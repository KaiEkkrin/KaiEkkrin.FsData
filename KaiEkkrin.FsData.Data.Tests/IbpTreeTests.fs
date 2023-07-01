namespace KaiEkkrin.FsData.Data.Tests

open System.Collections.Generic
open KaiEkkrin.FsData.Data
open Xunit

module IbpTreeTests =
    let emptyTree = IpbTree.createB<int, string>(4, Comparer<int>.Default)

    [<Fact>]
    let ``An empty tree contains nothing``() =
        let x = IpbTree.tryFind 4 emptyTree
        Assert.Equal (None, x)
