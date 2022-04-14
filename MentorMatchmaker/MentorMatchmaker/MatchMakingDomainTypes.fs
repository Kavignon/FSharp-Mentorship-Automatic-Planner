module MentorMatchmaker.DomainTypes

open System

open Utilities

type DayAvailability =
    { WeekDayName: string
      UtcHours: TimeSpan list }

type CalendarSchedule = { AvailableDays: DayAvailability nel }

type MentoringSession =
    { UtcStartTime: TimeSpan
      UtcEndTime: TimeSpan }

type OverlapSchedule =
    { Weekday: string
      MatchedAvailablePeriods: MentoringSession nel }

type PersonInformation =
    { Fullname: string
      SlackName: string
      EmailAddress: string
      MentorshipSchedule: CalendarSchedule }
    member x.FirstName = x.Fullname.Split(' ').[0]

type PopularityWeight =
    | Common = 3
    | Popular = 5
    | Rare = 10

type InterestCategory =
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
with
    member x.CategoryName =
        match x with
        | IntroductionToFSharp -> "Introduction to F#"
        | DeepDiveInFSharp -> "Deep dive in F#"
        | ContributeToOpenSource -> "Contribute to open-source"
        | ContributeToCompiler -> "Contribute to compiler"
        | WebDevelopment -> "Web development"
        | DistributedSystems -> "Distributed systems"
        | DomainModelling -> "Domain modeling"
        | MobileDevelopment -> "Mobile development"
        | MachineLearning -> "Machine learning"
        | DesigningWithTypes -> "Designing with types"
        | MetaProgramming -> "Meta programming"
        | UpForAnything -> "I am up for anything"
    member x.popularity = 
        match x with 
        | IntroductionToFSharp -> PopularityWeight.Common
        | DeepDiveInFSharp | ContributeToOpenSource | WebDevelopment | DomainModelling  -> PopularityWeight.Popular
        | _ -> PopularityWeight.Rare
    static member ofString (c: string) =
      let c= c.Trim()
      let inCategory  (keywordList: string) =
        let splitOptions = StringSplitOptions.RemoveEmptyEntries ||| StringSplitOptions.TrimEntries  
        keywordList.Split(";", splitOptions) 
        |> Array.exists (fun keyword -> c.Contains(keyword, StringComparison.InvariantCultureIgnoreCase))
      if inCategory  "Deep; dive; investment; better;" then DeepDiveInFSharp
      else if inCategory "Uno; Fabulous; Xamarin; Mobile; Mobile development" then MobileDevelopment
      else if inCategory "Microservices; Distributed systems; event sourcing" then DistributedSystems
      else if inCategory "Web;Elmish; Fable; SAFE; Giraffe; React; Feliz; MVC; Web development / SAFE stack;" then WebDevelopment
      else if inCategory "Contribute to an open source project; open source" then ContributeToOpenSource
      else
        match c with
          | "Introduction to F#" -> IntroductionToFSharp
          | "Deep dive in F#" -> DeepDiveInFSharp
          | "Contribute to open-source" -> ContributeToOpenSource
          | "Contribute to compiler" -> ContributeToCompiler
          | "Web development" -> WebDevelopment
          | "Distributed systems" -> DistributedSystems
          | "Domain modeling" -> DomainModelling
          | "Mobile development" -> MobileDevelopment
          | "Machine learning" -> MachineLearning
          | "Designing with types" -> DesigningWithTypes
          | "Meta programming" -> MetaProgramming
          | "I am up for anything" | _ -> UpForAnything




type InterestTopic =
    { Category: InterestCategory
      PopularityWeight: PopularityWeight }
    member x.Name = x.Category.CategoryName
    static member ofCategory c = 
      {Category = c
       PopularityWeight = c.popularity}

type Mentor =
    { MentorInformation: PersonInformation
      AreasOfExpertise: InterestTopic nel
      SimultaneousMenteeCount: int }

type Mentee =
    { MenteeInformation: PersonInformation
      TopicsOfInterest: InterestTopic nel }

type Applicants =
    { Mentees: Mentee list
      Mentors: Mentor list }

type PotentialMentorshipMatch =
    { Mentor: Mentor
      Mentee: Mentee
      MatchingFsharpInterests: InterestTopic list }

type ConfirmedMentorshipApplication =
    { MatchedMentee: Mentee
      MatchedMentor: Mentor
      CouldMentorHandleMoreWork: bool
      InterestTopics: InterestTopic list
      MeetingTimes: OverlapSchedule nel }

