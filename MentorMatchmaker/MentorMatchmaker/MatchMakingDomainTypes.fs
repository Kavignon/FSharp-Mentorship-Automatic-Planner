module MentorMatchmaker.DomainTypes

open System

open Utilities

type DayAvailability = { WeekDayName: string; UtcHours: TimeSpan list }
type CalendarSchedule = { AvailableDays: DayAvailability nel }
type MentoringSession = { UtcStartTime: TimeSpan; UtcEndTime: TimeSpan }
type OverlapSchedule = { Weekday: string; MatchedAvailablePeriods: MentoringSession nel }

type PersonInformation =
    { Fullname: string
      SlackName: string
      EmailAddress: string
      MentorshipSchedule: CalendarSchedule }

type FSharpCategory =
    | IntroductionToFSharp
    | DeepDiveInFSharp
    | ContributeToOpenSource
    | WebDevelopment
    | ContributeToCompiler
    | MachineLearning
    | UpForAnything
with
    member x.CategoryName =
        match x with
        | IntroductionToFSharp -> "Introduction to F#"
        | DeepDiveInFSharp -> "Deep dive in F#"
        | ContributeToOpenSource -> "Contribute to open-source"
        | ContributeToCompiler -> "Contribute to compiler"
        | WebDevelopment -> "Web development"
        | MachineLearning -> "Machine learning"
        | UpForAnything -> "I am up for anything"

type PopularityWeight =
    | Common = 3
    | Popular = 5
    | Rare = 10

type FsharpTopic =
    { Category: FSharpCategory
      PopularityWeight: PopularityWeight }
with
    member x.Name = x.Category.CategoryName

type Mentor =
    { MentorInformation: PersonInformation
      AreasOfExpertise: FsharpTopic nel
      SimultaneousMenteeCount: uint }

type Mentee =
    { MenteeInformation: PersonInformation
      TopicsOfInterest: FsharpTopic nel }

type PotentialMentorshipMatch =
    { Mentor: Mentor
      Mentee: Mentee
      MatchingFsharpInterests: FsharpTopic list }

type ConfirmedMentorshipApplication =
    { Mentee: Mentee
      Mentor: Mentor
      CouldMentorHandleMoreWork: bool
      FsharpTopic: FsharpTopic
      MeetingTimes: OverlapSchedule nel }