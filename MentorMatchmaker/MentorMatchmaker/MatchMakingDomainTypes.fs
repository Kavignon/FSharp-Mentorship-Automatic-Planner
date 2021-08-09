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
with
    member x.FirstName = x.Fullname.Split(' ').[0]

type FSharpCategory =
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
with
    member x.CategoryName =
        match x with
        | IntroductionToFSharp -> "Introduction to F#"
        | DeepDiveInFSharp -> "Deep dive in F#"
        | ContributeToOpenSource -> "Contribute to open-source"
        | ContributeToCompiler -> "Contribute to compiler"
        | WebDevelopment -> "Web development"
        | DistributedSystems -> "Distributed systems"
        | DomainModelling -> "Domain modelling"
        | MobileDevelopment -> "Mobile development"
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
    { MatchedMentee: Mentee
      MatchedMentor: Mentor
      CouldMentorHandleMoreWork: bool
      FsharpTopic: FsharpTopic list
      MeetingTimes: OverlapSchedule nel }

let introduction = { Category = IntroductionToFSharp; PopularityWeight = PopularityWeight.Common  }
let deepDive = { Category = DeepDiveInFSharp; PopularityWeight = PopularityWeight.Popular }
let contributeToOSS = { Category = ContributeToOpenSource; PopularityWeight = PopularityWeight.Popular }
let webDevelopment = { Category = WebDevelopment; PopularityWeight = PopularityWeight.Popular }
let contributeToCompiler = { Category = ContributeToCompiler; PopularityWeight = PopularityWeight.Rare }
let machineLearning = { Category = MachineLearning; PopularityWeight = PopularityWeight.Rare }
let upForAnything = { Category = UpForAnything; PopularityWeight = PopularityWeight.Rare }
let distributedSystems = { Category = DistributedSystems; PopularityWeight = PopularityWeight.Rare }
let mobileDevelopment = { Category = MobileDevelopment; PopularityWeight = PopularityWeight.Rare }
let domainModeling = { Category = DomainModelling; PopularityWeight = PopularityWeight.Popular }
