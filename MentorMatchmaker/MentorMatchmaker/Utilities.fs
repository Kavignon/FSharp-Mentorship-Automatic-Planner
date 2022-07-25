[<AutoOpen>]
module MentorMatchmaker.Utilities

open System
open System.Text.Json.Serialization

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

// Coming to .NET 7
// https://github.com/maxkoshevoi/DateOnlyTimeOnly.AspNet/blob/d239472270aaac83196a2ce1d701e60e3af670b9/DateOnlyTimeOnly.AspNet/Converters/Json/TimeOnlyJsonConverter.cs#L6
[<Sealed>]
type TimeOnlyJsonConverter() =
    inherit JsonConverter<TimeOnly>()

    override _.Read(reader, _, _) =
        TimeOnly.Parse(reader.GetString())

    override _.Write(writer, value, _) =
        let isoTime = value.ToString("O")
        writer.WriteStringValue(isoTime)
