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
    | WebDevelopment
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
let webDevelopment = { Category = WebDevelopment; PopularityWeight = Popular }
let contributeToCompiler = { Category = ContributeToCompiler; PopularityWeight = Rare }
let machineLearning = { Category = MachineLearning; PopularityWeight = Rare }
let upForAnything = { Category = UpForAnything; PopularityWeight = Rare }

let availableLocalTimeHoursForMentorship = [9..23] |> List.map(fun x -> TimeSpan(x, 0, 0))

let findTimeZone utcOffset =
    TimeZoneInfo.GetSystemTimeZones()
    |> Seq.filter(fun x -> x.BaseUtcOffset = utcOffset && x.SupportsDaylightSavingTime = true)
    |> Seq.head

let extractApplicantSchedule (row: MentorshipInformation.Row) =
    let convertAvailabilityIndexToTimeRange index =
        match index with
        | 0 -> Some (availableLocalTimeHoursForMentorship |> List.take 3)
        | 1 -> Some (availableLocalTimeHoursForMentorship |> List.skip 3 |> List.take 3)
        | 2 -> Some (availableLocalTimeHoursForMentorship |> List.skip 6 |> List.take 3)
        | 3 -> Some (availableLocalTimeHoursForMentorship |> List.skip 9 |> List.take 3)
        | 4 -> Some (availableLocalTimeHoursForMentorship |> List.skip 12 |> List.take 3)
        | _ -> None

    let convertDateAndTimeToAvailability weekDayAvailability availabilityIndex utcOffsetValue =
        if String.IsNullOrEmpty weekDayAvailability then None
        else
            let optAvailabilityRange = convertAvailabilityIndexToTimeRange availabilityIndex
            match optAvailabilityRange with
            | None -> None
            | Some availabilityRange ->
                let availableRangeInUtc = availabilityRange |> List.map(fun x -> x.Add(utcOffsetValue))
                if weekDayAvailability.Contains ',' <> true then
                    Some [ { WeekDayName = weekDayAvailability; UtcHours = availableRangeInUtc } ]
                else
                    weekDayAvailability.Split(',')
                    |> List.ofArray
                    |> List.map(fun x -> x.Replace(" ", ""))
                    |> List.map(fun x -> { WeekDayName = x; UtcHours = availableRangeInUtc })
                    |> Some

    // The timezone, as mentioned in the CSV data, is in fact a UTC offset, not a proper TZ
    let utcOffset = 
        if row.``What is your time zone?``.Equals "UTC" then
            TimeSpan(0,0,0)
        else
            let normalizedUtcValue = row.``What is your time zone?``.Replace("UTC", "").Replace("+", "").Replace(" ", "")
            TimeSpan(Int32.Parse(normalizedUtcValue), 0, 0)

    let applicantTimeZone = findTimeZone utcOffset
    let availableDays =
        [
            row.``What time are you available? [09:00 - 12:00 local time]``
            row.``What time are you available? [12:00 - 15:00 local time]``
            row.``What time are you available? [15:00 - 18:00 local time]``
            row.``What time are you available? [18:00 - 21:00 local time]``
            row.``What time are you available? [21:00 - 00:00 local time]``
        ]
        |> List.mapi(fun idx dateAndTime -> convertDateAndTimeToAvailability dateAndTime idx utcOffset)
        |> List.choose(fun x -> x)
        |> List.concat
        |> List.groupBy(fun x -> x.WeekDayName)
        |> List.map(fun x -> 
            let utcHours =
                (snd x)
                |> List.map(fun availableDay -> availableDay.UtcHours)
                |> List.concat

            { WeekDayName = fst x; UtcHours = utcHours }
        )


    let availableDay = { WeekDayName = "Wtv"; UtcHours = [] }

    { AvailableDays = NonEmptyList.create availableDays.Head availableDays.Tail}

let extractApplicantInformation (row: MentorshipInformation.Row) =
    { 
        Fullname = row.``What is your full name (First and Last Name)``
        SlackName = row.``What is your fsharp.org slack name?``
        EmailAddress = row.``Email Address``
        AvailableScheduleForMentorship = extractApplicantSchedule row
    }

let extractFsharpTopic (row: MentorshipInformation.Row) =
    let convertCategoryNameToTopic categoryName =
        if String.IsNullOrEmpty categoryName then None
        else
            match categoryName with
            | "Introduction to F#" -> Some introduction
            | "Deep dive into F#" -> Some deepDive
            | "Contribute to an open source project" -> Some contributeToOSS
            | "Machine learning" -> Some machineLearning
            | "Contribute to the compiler" -> Some contributeToCompiler
            | "Web and SAFE stack"
            | "Web programming/SAFE stack"
            | "Fable/Elmish"
            | "web development"
            | "Web development" -> Some webDevelopment
            | "I am up for anything"
            | _ -> Some upForAnything

    if String.IsNullOrEmpty row.``What topic do you want to learn?`` then
        convertCategoryNameToTopic row.``What topics do you feel comfortable mentoring?``
    else
        convertCategoryNameToTopic row.``What topic do you want to learn?``
        
let extractPeopleInformation (mentorshipDocument: MentorshipInformation) =
    let extractFsharpTopics (rows: MentorshipInformation.Row seq) =
        let fsharpTopicsList = 
            rows
            |> Seq.map extractFsharpTopic
            |> Seq.choose(fun x -> x)
            |> List.ofSeq
        NonEmptyList.create fsharpTopicsList.Head fsharpTopicsList.Tail

    let mentors =
        mentorshipDocument.Rows
        |> Seq.filter (fun row -> String.IsNullOrEmpty(row.``What topics do you feel comfortable mentoring?``) <> true)
        |> Seq.groupBy(fun row -> row.``What is your full name (First and Last Name)``)
        |> Seq.map(fun x -> 
            let multipleMentorEntries = snd x
            let mentorData = Seq.head multipleMentorEntries
            { 
                MentorInformation = extractApplicantInformation mentorData
                SimultaneousMenteeCount = multipleMentorEntries |> Seq.length |> uint
                AreasOfExpertise = extractFsharpTopics multipleMentorEntries
            }
        )
    
    let mentees =
        mentorshipDocument.Rows
        |> Seq.filter (fun row -> String.IsNullOrEmpty(row.``What topic do you want to learn?``) <> true)
        |> Seq.groupBy(fun row -> row.``What is your full name (First and Last Name)``)
        |> Seq.map(fun row ->
            let multipleMenteeEntries = snd row
            let menteeData =  Seq.head multipleMenteeEntries
            {
                MenteeInformation = extractApplicantInformation menteeData
                TopicsOfInterest = extractFsharpTopics multipleMenteeEntries
            }
        )

    (mentors, mentees)
        

[<EntryPoint>]
let main argv =
    let peopleInformation = 
        MentorshipInformation.GetSample() 
        |> extractPeopleInformation
        ||> Seq.iter2(fun mentor mentee -> 
            printfn $"Mentor: {mentor.MentorInformation.Fullname} with {mentor.SimultaneousMenteeCount} students."
            printfn $"Mentee: {mentee.MenteeInformation.Fullname} with {mentee.TopicsOfInterest.Length} interests."
        )

    ////|> matchMentorToMentee
    ////|> generateOrganizationEmail
    ////|> outputMailToLinkInHtml

    0 // return an integer exit code
