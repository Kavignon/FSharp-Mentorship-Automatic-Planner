module MentorMatchmaker.Tests.UtilitiesTests

open MentorMatchmaker.Utilities
open FsUnit
open NUnit.Framework

[<Test>]
let ``Set.overlaps returns true if both sets contain a similar item``() =
    Set.overlaps (Set [1..10]) (Set [10..20]) |> should be True

[<Test>]
let ``Set.overlaps returns false if each set has distinct items from the other``() =
    Set.overlaps (Set [1..10]) (Set [11..20]) |> should be False

[<Test>]
