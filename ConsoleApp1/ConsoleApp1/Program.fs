open System
open FSharp.Data
open FSharpx.Collections

type MentorshipInformation = CsvProvider<"Mentorship Application Fall Session 2020 (Responses).csv">

type DayAvailability = {
    WeekDayName: string
    UtcHours: TimeSpan list
}

type CalendarSchedule = {
    AvailableDays: NonEmptyList<DayAvailability>
}

type PersonInformation = {
    Fullname: string
    SlackName: string
    EmailAddress: string
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
    TopicsOfInterest: NonEmptyList<FsharpTopic>
}

type MentorshipMatches = Map<Mentor, NonEmptyList<Mentee>>

let introduction = { Category = IntroductionToFSharp; PopularityWeight = Common }
let deepDive = { Category = DeepDiveInFSharp; PopularityWeight = Popular }
let contributeToOSS = { Category = ContributeToOpenSource; PopularityWeight = Popular }
let contributeToCompiler = { Category = ContributeToCompiler; PopularityWeight = Rare }
let machineLearning = { Category = MachineLearning; PopularityWeight = Rare }
let upForAnything = { Category = UpForAnything; PopularityWeight = Rare }

let availableLocalTimeHoursForMentorship = [9..23] |> List.map(fun x -> TimeSpan(x, 0, 0))

let filterAndGroupApplicantsByNames 
    (applicants: MentorshipInformation.Row seq) 
    (predicate: MentorshipInformation.Row -> bool) =
    applicants
    |> Seq.filter predicate
    |> Seq.groupBy(fun row -> row.``What is your full name (First and Last Name)``)

let findTimeZone utcOffset =
    TimeZoneInfo.GetSystemTimeZones()
    |> Seq.filter(fun x -> x.BaseUtcOffset = utcOffset && x.SupportsDaylightSavingTime = true)
    |> Seq.head

let extractApplicantSchedule (row: MentorshipInformation.Row) =
    // The timezone, as mentioned in the CSV data, is in fact a UTC offset, not a proper TZ.S
    let utcOffset = TimeSpan(Int32.Parse(row.``What is your time zone?``.Replace("UTC", "")), 0, 0)
    let applicantTimeZone = findTimeZone utcOffset

    let availableDay = { WeekDayName = "Wtv"; UtcHours = [] }

    { AvailableDays = NonEmptyList.create availableDay [] }

let extractApplicantInformation (row: MentorshipInformation.Row) =
    { 
        Fullname = row.``What is your full name (First and Last Name)``
        SlackName = row.``What is your fsharp.org slack name?``
        EmailAddress = row.``Email Address``
        AvailableScheduleForMentorship = extractApplicantSchedule row
    }    

let extractPeopleInformation (mentorshipDocument: MentorshipInformation) =
    let mentors =
        (mentorshipDocument.Rows, fun row -> String.IsNullOrEmpty(row.``What topics do you feel comfortable mentoring?``) <> true)
        ||> filterAndGroupApplicantsByNames
        |> Seq.map(fun x -> 
            let multipleMentorEntries = snd x
            let mentorData = Seq.head multipleMentorEntries 

            { 
                MentorInformation = extractApplicantInformation mentorData
                SimultaneousMenteeCount = multipleMentorEntries |> Seq.length |> uint
                AreasOfExpertise = 
            }
        )
    
    let mentees =
        (mentorshipDocument.Rows, fun row -> String.IsNullOrEmpty(row.``What topic do you want to learn?``) <> true)
        ||> filterAndGroupApplicantsByNames
        |> Seq.map(fun row ->
            let multipleMenteeEntries = snd x
            let menteeData =  Seq.head multipleMenteeEntries
            {
                MenteeInformation = extractApplicantInformation menteeData
            }
        )

    (mentors, mentees)
        

[<EntryPoint>]
let main argv =
    MentorshipInformation.GetSample()
    |> extractPeopleInformation
    |> matchMentorToMentee
    |> generateOrganizationEmail
    |> outputMailToLinkInHtml

    0 // return an integer exit code
