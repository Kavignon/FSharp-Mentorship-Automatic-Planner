[<AutoOpen>]
module MentorMatchmaker.Utilities

open System.Linq

open FSharpPlus.Data

type 'T nel = 'T NonEmptyList

[<RequireQualifiedAccess>]
module NonEmptyList =
    let intersect (a: _ nel) (b: _ nel) =
        Enumerable.Intersect(a, b) |> List.ofSeq

[<RequireQualifiedAccess>]
module List =
    let intersect (a: _ list) (b: _ list) =
        Enumerable.Intersect(a, b) |> List.ofSeq

    let isNotEmpty l = l |> List.isEmpty |> not

[<RequireQualifiedAccess>]
module Set =
    /// Tests if any item in one set is in the other set
    let overlaps set1 set2 =
        let biggerSet, smallerSet =
            if Set.count set1 > Set.count set2 then
                set1, set2
            else
                set2, set1
        Set.exists (fun item -> Set.contains item biggerSet) smallerSet
