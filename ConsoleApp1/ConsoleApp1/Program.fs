// Learn more about F# at http://fsharp.org

open System
open System.ComponentModel.DataAnnotations

open FSharp.Data
open FSharpx.Collections

type MentorshipInformation = CsvProvider<"Mentorship Application Fall Session 2020 (Responses).csv">

type EmailCreationError =
    | MissingOrEmptyInput
    | EmailAddressInvalid of string
with
    member x.ErrorMessage =
        match x with
        | MissingOrEmptyInput -> "The input was either missing or empty. Please provide an email address for the customer."
        | EmailAddressInvalid address -> $"The provided email address {address} was invalid. Please provide a valid email address."

/// Abstract away the email contact information of a customer.
type EmailAddress = private EmailContact of string
with
    static member Create addressInput =
        if String.IsNullOrEmpty addressInput then
            Error MissingOrEmptyInput
        elif EmailAddressAttribute().IsValid(addressInput) <> true then
            Error (EmailAddressInvalid $"The following email input isn't valid {addressInput}.")
        else
            Ok (EmailContact addressInput)

    member x.Value =
        match x with
        | EmailContact emailAddress -> emailAddress

type DayAvailability = {
    WeekDayName: string
    LocalAvailableHours: int list
}

type CalendarSchedule = {
    UtcOffset: string
    AvailableDays: NonEmptyList<DayAvailability>
}

type PersonInformation = {
    Fullname: string
    SlackName: string
    EmailAddress: EmailAddress
    AvailableScheduleForMentorship: CalendarSchedule
}

type FSharpCategory =
    | IntroductionToFSharp
    | DeepDiveInFSharp
    | ContributeToOpenSource
    | ContributeToCompiler
    | MachineLearning
    | UpForAnything

// Heuristic type used to associate a mentor which can help in more sought for topics such as Machine Learning.
type PopularityHeuristic =
    | Common
    | Popular
    | Rare

type FsharpTopic = {
    Category: FSharpCategory
    PopularityWeight: PopularityHeuristic
}

type Mentor = {
    MentorInformation: PersonInformation
    AreasOfExpertise: NonEmptyList<FsharpTopic>
    SimultaneousMenteeCount: uint
}

type Mentee = {
    MenteeInformation: PersonInformation
    TopicOfInterest: FsharpTopic
}

type MentorshipMatches = Map<Mentor, NonEmptyList<Mentee>>

let introduction = { Category = IntroductionToFSharp; PopularityWeight = Common }
let deepDive = { Category = DeepDiveInFSharp; PopularityWeight = Popular }
let contributeToOSS = { Category = ContributeToOpenSource; PopularityWeight = Popular }
let contributeToCompiler = { Category = ContributeToCompiler; PopularityWeight = Rare }
let machineLearning = { Category = MachineLearning; PopularityWeight = Rare }
let upForAnything = { Category = UpForAnything; PopularityWeight = Rare }



[<EntryPoint>]
let main argv =
    let mentorshipData = new MentorshipInformation()
    mentorshipData.Rows |> Seq.iteri(fun index row -> row. printfn $"#{index} - {row.``What is your time zone?``}")

    0 // return an integer exit code
