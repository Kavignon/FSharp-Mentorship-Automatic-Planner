module MentorMatchmaker.Tests.DomainTests

open MentorMatchmaker.Domain
open System
open type DayOfWeek
open NUnit.Framework
open FsUnit

[<Test>]
let ``WeekTime.Add Increments Hours Correctly ``() =
    { Weekday = Sunday; Time = TimeOnly(12, 0, 0) }.AddHours(1)
    |> should equal { Weekday = Sunday; Time = TimeOnly(13, 0, 0) }

[<Test>]
let ``WeekTime.Add Decrements Hours Correctly ``() =
    { Weekday = Sunday; Time = TimeOnly(12, 0, 0) }.AddHours(-1)
    |> should equal { Weekday = Sunday; Time = TimeOnly(11, 0, 0) }

[<Test>]
let ``WeekTime.Add Decrements weeks worth of Hours Correctly ``() =
    { Weekday = Sunday; Time = TimeOnly(12, 0, 0) }.AddHours(24.0 * -15.0)
    |> should equal { Weekday = DayOfWeek.Saturday; Time = TimeOnly(12, 0, 0) }

[<Test>]
let ``WeekTime.Add increments to next day of week``() =
    { Weekday = Monday; Time = TimeOnly(0, 0, 0) }.AddHours(24)
    |> should equal { Weekday = Tuesday; Time = TimeOnly(0, 0, 0) }

[<Test>]
let ``WeekTime.Add decrements to previous day of week``() =
    { Weekday = Monday; Time = TimeOnly(0, 0, 0) }.AddHours(-24)
    |> should equal { Weekday = Sunday; Time = TimeOnly(0, 0, 0) }

[<Test>]
let ``WeekTime.Add increments from Saturday to Sunday``() =
    { Weekday = Saturday; Time = TimeOnly(0, 0, 0) }.AddHours(24)
    |> should equal { Weekday = Sunday; Time = TimeOnly(0, 0, 0) }

[<Test>]
let ``WeekTime.Add decrements from Sunday to Saturday``() =
    { Weekday = Sunday; Time = TimeOnly(0, 0, 0) }.AddHours(-24)
    |> should equal { Weekday = Saturday; Time = TimeOnly(0, 0, 0) }

[<Test>]
let ``Converting a Set of WeekTimes to a Sequence of WeekTimeRange``() =
    Set [
        for i in 1..10 do yield { Weekday = Sunday; Time = TimeOnly(i,0,0) }
        for i in 5..7 do yield { Weekday = Monday; Time = TimeOnly(i,0,0) }
        for i in 13..23 do yield { Weekday = Tuesday; Time = TimeOnly(i,0,0) }
        // Intentional Duplicate Sunday
        for i in 11..15 do yield { Weekday = Sunday; Time = TimeOnly(i,0,0) }
    ]
    |> toWeekTimeRanges
    |> should equivalent [
        { Start = { Weekday = Sunday; Time = TimeOnly(1, 0, 0) }
          End = { Weekday = Sunday; Time = TimeOnly(15, 0, 0) } }
        { Start = { Weekday = Monday; Time = TimeOnly(5, 0, 0) }
          End = { Weekday = Monday; Time = TimeOnly(7, 0, 0) } }
        { Start = { Weekday = Tuesday; Time = TimeOnly(13, 0, 0) }
          End = { Weekday = Tuesday; Time = TimeOnly(23, 0, 0) } }
    ]
