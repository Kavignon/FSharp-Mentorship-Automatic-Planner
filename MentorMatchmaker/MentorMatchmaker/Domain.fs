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

type PersonalInformation =
    { FullName: string
      SlackName: string
      EmailAddress: string
      LocalOffset: int }

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
