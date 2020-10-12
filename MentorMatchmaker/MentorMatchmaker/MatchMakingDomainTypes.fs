module MentorMatchmaker.DomainTypes

open System

open Utilities

type DayAvailability = { WeekDayName: string; UtcHours: TimeSpan list }
type CalendarSchedule = { AvailableDays: DayAvailability nel }
type TimeRange = { UtcStartTime: TimeSpan; UtcEndTime: TimeSpan }
type OverlapSchedule = { Weekday: string; MatchPeriods: TimeRange nel }

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

type PopularityWeight =
    | Common = 3
    | Popular = 5
    | Rare = 10

type FsharpTopic =
    { Category: FSharpCategory
      PopularityWeight: PopularityWeight }

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
      FsharpTopic: FsharpTopic
      MeetingTimes: OverlapSchedule nel }