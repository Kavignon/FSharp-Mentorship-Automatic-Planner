open System
open System.Linq
open FSharp.Data
open FSharpx.Collections

type MentorshipInformation = CsvProvider<"data.csv">

type 'T nel = 'T NonEmptyList

type DayAvailability =
    { WeekDayName: string
      UtcHours: TimeSpan list }

let checkForAvailabilityMatch (mentorAvailability: DayAvailability) (menteeAvailability: DayAvailability) =
    mentorAvailability.WeekDayName.Equals(menteeAvailability.WeekDayName) &&
    Enumerable.Intersect(menteeAvailability.UtcHours, mentorAvailability.UtcHours).Count() >= 2

type CalendarSchedule =
    { AvailableDays: DayAvailability nel }

type TimeRange =
    { UtcStartTime: TimeSpan
      UtcEndTime: TimeSpan }

type OverlapSchedule =
    { Weekday: string
      MatchPeriods: TimeRange nel }

type PersonInformation =
    { Fullname: string
      SlackName: string
      EmailAddress: string
      AvailableScheduleForMentorship: CalendarSchedule }

type FSharpCategory =
    | IntroductionToFSharp
    | DeepDiveInFSharp
    | ContributeToOpenSource
    | WebDevelopment
    | ContributeToCompiler
    | MachineLearning
    | UpForAnything

// Heuristic type used to associate a mentor which can help in more sought for topics such as Machine Learning.
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

type ConfirmedMentorshipApplication =
    { Mentee: Mentee
      Mentor: Mentor
      FsharpTopic: FsharpTopic
      MeetingTimes: OverlapSchedule nel }

let introduction = { Category = IntroductionToFSharp; PopularityWeight = PopularityWeight.Common  }
let deepDive = { Category = DeepDiveInFSharp; PopularityWeight = PopularityWeight.Popular }
let contributeToOSS = { Category = ContributeToOpenSource; PopularityWeight = PopularityWeight.Popular }
let webDevelopment = { Category = WebDevelopment; PopularityWeight = PopularityWeight.Popular }
let contributeToCompiler = { Category = ContributeToCompiler; PopularityWeight = PopularityWeight.Rare }
let machineLearning = { Category = MachineLearning; PopularityWeight = PopularityWeight.Rare }
let upForAnything = { Category = UpForAnything; PopularityWeight = PopularityWeight.Rare }

let availableLocalTimeHoursForMentorship = [9..23] |> List.map(fun x -> TimeSpan(x, 0, 0))

let findTimeZone utcOffset =
    TimeZoneInfo.GetSystemTimeZones()
    |> Seq.filter(fun x -> x.BaseUtcOffset = utcOffset && x.SupportsDaylightSavingTime = true)
    |> Seq.head // Flaw of implementation and in the data. We only have UTC offset instead of time zones.

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
                let availableRangeInUtc = availabilityRange |> List.map(fun x -> x.Subtract(utcOffsetValue))
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
                x
                |> snd
                |> List.map(fun availableDay -> availableDay.UtcHours)
                |> List.concat

            { WeekDayName = fst x; UtcHours = utcHours }
        )

    { AvailableDays = NonEmptyList.create availableDays.Head availableDays.Tail}

let extractApplicantInformation (row: MentorshipInformation.Row) =
    { Fullname = row.``What is your full name (First and Last Name)``
      SlackName = row.``What is your fsharp.org slack name?``
      EmailAddress = row.``Email Address``
      AvailableScheduleForMentorship = extractApplicantSchedule row }

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
        rows
        |> List.ofSeq
        |> List.map extractFsharpTopic
        |> List.choose(fun x -> x)
        |> List.sortByDescending(fun x -> x.PopularityWeight)
        |> NonEmptyList.ofList

    let mentors =
        mentorshipDocument.Rows
        |> List.ofSeq
        |> List.filter (fun row -> String.IsNullOrEmpty(row.``What topics do you feel comfortable mentoring?``) <> true)
        |> List.groupBy(fun row -> row.``What is your full name (First and Last Name)``)
        |> List.map(fun x ->
            let multipleMentorEntries = snd x
            let mentorData = List.head multipleMentorEntries
            { MentorInformation = extractApplicantInformation mentorData
              SimultaneousMenteeCount = multipleMentorEntries |> Seq.length |> uint
              AreasOfExpertise = extractFsharpTopics multipleMentorEntries })

    let mentees =
        mentorshipDocument.Rows
        |> List.ofSeq
        |> List.filter (fun row -> String.IsNullOrEmpty(row.``What topic do you want to learn?``) <> true)
        |> List.groupBy(fun row -> row.``What is your full name (First and Last Name)``)
        |> List.map(fun row ->
            let multipleMenteeEntries = snd row
            let menteeData = List.head multipleMenteeEntries
            { MenteeInformation = extractApplicantInformation menteeData
              TopicsOfInterest = extractFsharpTopics multipleMenteeEntries })

    (mentors, mentees)

let doScheduleOverlap (menteeSchedule: CalendarSchedule) (mentorSchedule: CalendarSchedule) =
    let menteeAvailabilities = menteeSchedule.AvailableDays |> NonEmptyList.toList
    let mentorAvailabilities = mentorSchedule.AvailableDays |> NonEmptyList.toList
    let isThereAnAvailabilityBetweenApplicants menteeSchedule =
        List.exists(fun mentorSchedule -> checkForAvailabilityMatch menteeSchedule mentorSchedule) mentorAvailabilities

    List.exists(fun menteeSchedule -> isThereAnAvailabilityBetweenApplicants menteeSchedule) menteeAvailabilities

let findMatchingMenteeForMentor (mentor: Mentor) (mentees: Mentee list) =
    let fromRarestToCommonExpertiseAreas = mentor.AreasOfExpertise |> NonEmptyList.toList |> List.sortByDescending(fun x -> x.PopularityWeight)

    fromRarestToCommonExpertiseAreas
    |> List.map(fun expertiseArea ->
        mentees |> List.map(fun mentee ->
        let foundScheduleOverlap = (mentee.MenteeInformation.AvailableScheduleForMentorship, mentor.MentorInformation.AvailableScheduleForMentorship) ||> doScheduleOverlap
        let foundMatchingMentee = foundScheduleOverlap && mentee.TopicsOfInterest.Contains expertiseArea

        if foundMatchingMentee
        then Some (expertiseArea, mentee)
        else None)
    )
    |> List.concat
    |> List.choose(fun x -> x)
    |> List.sortByDescending(fun (topic, _) -> topic.PopularityWeight)

module List =
    let intersect (a: _ list) (b: _ list) = Enumerable.Intersect(a, b) |> List.ofSeq

    let toConsecutivePairs (l: _ list) =
        l
        |> List.windowed 2
        |> List.map(fun arrayPair -> (arrayPair.[0], arrayPair.[1]))

let generateMeetingTimes mentorSchedule menteeSchedule =
    let sortByWeekDayName nonEmptyAvailableDays =
        nonEmptyAvailableDays
        |> NonEmptyList.toList
        |> List.sortBy(fun x -> x.WeekDayName)

    let mentorAvailableDaysList = sortByWeekDayName mentorSchedule.AvailableDays
    let menteeAvailableDaysList = sortByWeekDayName menteeSchedule.AvailableDays

    (menteeAvailableDaysList, mentorAvailableDaysList)
    ||> List.map2(fun menteeAvailableDay mentorAvailableDay ->
        let sameAvailableHours =
            List.intersect menteeAvailableDay.UtcHours mentorAvailableDay.UtcHours
            |> List.toConsecutivePairs
            |> List.filter(fun (previousHour, currentHour) -> previousHour.Hours + 1 = currentHour.Hours)
            |> List.map(fun (previousHour, currentHour) -> { UtcStartTime = previousHour; UtcEndTime = currentHour })

        if sameAvailableHours.Length = 0 then
            None
        else
            Some { Weekday = menteeAvailableDay.WeekDayName; MatchPeriods = NonEmptyList.ofList sameAvailableHours }
    )
    |> List.choose(fun x -> x)

let findMentorMatchingMenteeInterest listOfMentors listOfMentees =
    listOfMentors
    |> List.map(fun mentor ->
        let menteeMatches = (mentor, listOfMentees) ||> findMatchingMenteeForMentor
        if menteeMatches.Length = 0 then None
        else
            let fsharpTopicAndMenteeTuple = menteeMatches.Head
            let matchedMentee = snd fsharpTopicAndMenteeTuple
            let fsharpTopic = fst fsharpTopicAndMenteeTuple
            let matchingSchedule =  generateMeetingTimes matchedMentee.MenteeInformation.AvailableScheduleForMentorship mentor.MentorInformation.AvailableScheduleForMentorship
            Some {
                Mentee = matchedMentee
                Mentor = mentor
                FsharpTopic = fsharpTopic
                MeetingTimes = NonEmptyList.create matchingSchedule.Head matchingSchedule.Tail
            }
    )
    |> List.choose(fun x -> x)

let canMatchMentorToMentee listOfMentors listOfMentees =
    listOfMentors
    |> List.exists(fun mentor ->
        let menteeMatches = (mentor, listOfMentees) ||> findMatchingMenteeForMentor
        menteeMatches.Length > 0
    )

let rec matchMenteeToMentor
    (matches: ConfirmedMentorshipApplication list)
    (mentees: Mentee list)
    (mentors: Mentor list)
    : ConfirmedMentorshipApplication list =
    match(mentees, mentors) with
    | ([], _) -> matches
    | (_, []) -> matches
    | (listOfMentees, listOfMentors) ->
        let atLeastOneMatchPossible = canMatchMentorToMentee listOfMentors listOfMentees
        if atLeastOneMatchPossible <> true then
            matches
        else
            let mentorshipMatches = findMentorMatchingMenteeInterest listOfMentors listOfMentees
            let currentMentors = listOfMentors |> List.filter(fun x -> mentorshipMatches |> List.exists(fun y -> x <> y.Mentor))
            let currentMentees = listOfMentees |> List.filter(fun x -> mentorshipMatches |> List.exists(fun y -> x <> y.Mentee))

            matchMenteeToMentor (matches @ mentorshipMatches) currentMentees currentMentors

[<EntryPoint>]
let main argv =
    // Don't forget to provide the current CSV document for the mentorship.
    // Please leave the CSV document out of the repository. It's been excluded in the git ignore.
    // Don 't commit the file in the repository.
    let (mentors, mentees) = extractPeopleInformation (MentorshipInformation.GetSample())
    let mentorshipConfirmedMatchedApplicants = matchMenteeToMentor [] mentees mentors
    ////|> generateOrganizationEmail
    ////|> outputMailToLinkInHtml

    0 // return an integer exit code
