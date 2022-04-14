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

    let toConsecutivePairs (l: _ list) =
        l
        |> List.windowed 2
        |> List.map (fun arrayPair -> (arrayPair.[0], arrayPair.[1]))

    let chooseDefault l = l |> List.choose id

    let isNotEmpty l = l |> List.isEmpty |> not
