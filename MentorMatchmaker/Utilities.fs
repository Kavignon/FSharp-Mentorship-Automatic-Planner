﻿[<AutoOpen>]
module MentorMatchmaker.Utilities

open System

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

let (|RegexGroupValue|_|) pattern text =
    let m = System.Text.RegularExpressions.Regex.Match(text, pattern)
    if m.Success && m.Groups.Count > 1 then
        Some m.Groups[1].Value
    else
        None

[<return:Struct>]
let (|IgnoreCase|_|) other text =
    if String.Equals(text, other, StringComparison.InvariantCultureIgnoreCase) then
        ValueSome()
    else
        ValueNone

let (|Positive|Negative|Zero|) n =
    if n > 0 then Positive n
    elif n < 0 then Negative n
    else Zero
