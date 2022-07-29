[<AutoOpen>]
module MentorMatchmaker.Domain

open System

type WeekTime = {
    Weekday: DayOfWeek
    Time: TimeOnly
} with
    member this.AddHours(hours) =
        let time, dayOffset = this.Time.AddHours(hours)
        if dayOffset = 0 then
            { this with Time = time }
        else
            let weekday = ((int this.Weekday + dayOffset) % 7 + 7) % 7 |> enum<DayOfWeek>
            { this with Time = time; Weekday = weekday }

type WeekTimeRange =
    { Start: WeekTime
      End: WeekTime }
with
    override this.ToString() =
        $"{this.Start.Weekday}: {this.Start.Time:ht}-{this.End.Time:ht}"

type PersonalInformation =
    { FullName: string
      SlackName: string
      EmailAddress: string
      LocalOffset: int }
    member this.FirstName = this.FullName.Split(' ')[0]

type Topic =
    | IntroductionToFSharp
    | DeepDiveInFSharp
    | ContributeToOpenSource
    | WebDevelopment
    | ContributeToCompiler
    | MachineLearning
    | DistributedSystems
    | MobileDevelopment
    | DomainModelling
    | UpForAnything
    | DesigningWithTypes
    | MetaProgramming

type Applicant =
    { PersonalInformation: PersonalInformation
      Availabilities: WeekTimeRange list
      Topics: Set<Topic> }

type ApplicantPool =
    { Mentors: Applicant list
      Mentees: Applicant list }

type MentorshipPair =
    { Mentor: Applicant
      Mentee: Applicant
      MutualAvailabilities: Set<WeekTime>
      MutualTopics: Set<Topic> }


let toWeekTimes (weekTimeRanges: WeekTimeRange list) = Set [
    for { Start = start; End = end' } in weekTimeRanges do
        let mutable currentWeekTime = start
        yield currentWeekTime
        while currentWeekTime < end' do
            currentWeekTime <- currentWeekTime.AddHours(1)
            yield currentWeekTime
]

let toWeekTimeRanges (weekTimes: Set<WeekTime>) =
    weekTimes
    |> Seq.sortBy (fun weekTime -> weekTime.Weekday, weekTime.Time)
    |> Seq.groupBy (fun weekTime -> weekTime.Weekday)
    |> Seq.collect (fun (weekday, weekTimes) -> seq {
        use e = weekTimes.GetEnumerator()
        e.MoveNext() |> ignore
        let mutable min = e.Current.Time.Hour
        let mutable previous = e.Current.Time.Hour
        let mutable current = e.Current.Time.Hour
        while e.MoveNext() do
            previous <- current
            current <- e.Current.Time.Hour

            if current - previous > 1 then
                yield { Start = { Weekday = weekday; Time = TimeOnly(min, 0, 0) }
                        End = { Weekday = weekday; Time = TimeOnly(previous, 0, 0) } }

        yield { Start = { Weekday = weekday; Time = TimeOnly(min, 0, 0) }
                End = { Weekday = weekday; Time = TimeOnly(current, 0, 0) } }

    })
    |> Seq.toList
