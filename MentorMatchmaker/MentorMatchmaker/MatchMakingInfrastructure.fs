module MentorMatchmaker.Infra

open System

open FSharp.Data
open FSharpPlus.Data

open Utilities
open DomainTypes

type MentorshipInformation = CsvProvider<"mentorship_schema_file.csv">

type MentorshipPlannerInputs = 
    { Applicants: Applicants
      ConfirmedMatches: ConfirmedMentorshipApplication list
      MatchedMenteesSet: Set<Mentee>
      MatchedMentorSet: Set<Mentor>
      NumberOfHoursRequiredForOverlap: int }

module private Impl =
    let availableLocalTimeHoursForMentorship =
        [ 9 .. 23 ]
        |> List.map (fun x -> TimeSpan(x, 0, 0))

    let splitStringAndRemoveDelimiters (stringInput: string) =
        stringInput.Split([|','; ';'|])
        |> List.ofArray
        |> List.map (fun x -> x.Replace(" ", ""))

    let distributeOffsetHoursToSeparateDays
        (dayOfTheWeekName: string)
        (availableHoursUtc: TimeSpan list)
        : DayAvailability list =
        let offsetGivenDayByOne (givenDay: DayOfWeek) (isCheckingForNextDay: bool) =
            if isCheckingForNextDay
               && givenDay = DayOfWeek.Saturday then
                DayOfWeek.Sunday
            elif isCheckingForNextDay <> true
                 && givenDay = DayOfWeek.Sunday then
                DayOfWeek.Saturday
            else
                let offsetValue = if isCheckingForNextDay then 1 else -1
                enum<DayOfWeek> (int givenDay + 1)

        let convertWeekDayNameInDayOfWeek weekDayName =
            match weekDayName with
            | "Sunday" -> DayOfWeek.Sunday
            | "Monday" -> DayOfWeek.Monday
            | "Tuesday" -> DayOfWeek.Tuesday
            | "Wednesday" -> DayOfWeek.Wednesday
            | "Thursday" -> DayOfWeek.Thursday
            | "Friday" -> DayOfWeek.Friday
            | "Saturday" -> DayOfWeek.Saturday

        let generateMeetingHoursForOffsetDay (offsetDayMeetingHours: TimeSpan list) (isCheckingForNextDay: bool) =
            let dayOfTheWeek =
                convertWeekDayNameInDayOfWeek dayOfTheWeekName

            let nextDay =
                offsetGivenDayByOne dayOfTheWeek isCheckingForNextDay

            { WeekDayName = nextDay.ToString()
              UtcHours = offsetDayMeetingHours }

        let negativeOffsets =
            availableHoursUtc
            |> List.filter (fun x -> x.Days < 0)

        let positiveOffsets =
            availableHoursUtc
            |> List.filter (fun x -> x.Days >= 1 && x.Hours >= 0)

        match (positiveOffsets, negativeOffsets) with
        | [], [] ->
            [ { WeekDayName = dayOfTheWeekName
                UtcHours = availableHoursUtc } ]

        | nextDayHours, [] when nextDayHours.Length > 0 ->
            let nextDayMeetingHoursWithoutOffset =
                nextDayHours
                |> List.map (fun x -> TimeSpan(x.Hours, x.Minutes, x.Seconds))

            let currentDayMeetingHours =
                availableHoursUtc
                |> List.filter (fun x -> x.Days = 0)

            [ { WeekDayName = dayOfTheWeekName
                UtcHours = currentDayMeetingHours }
              generateMeetingHoursForOffsetDay nextDayMeetingHoursWithoutOffset true ]

        | [], previousDayHours when previousDayHours.Length > 0 ->
            let previousDayMeetingHoursWithoutOffset =
                previousDayHours
                |> List.map (fun x -> TimeSpan(x.Hours, x.Minutes, x.Seconds))

            let currentDayMeetingHours =
                availableHoursUtc
                |> List.filter (fun x -> x.Days = 0)

            [ generateMeetingHoursForOffsetDay previousDayMeetingHoursWithoutOffset false
              { WeekDayName = dayOfTheWeekName
                UtcHours = currentDayMeetingHours } ]

        | nextDaysHours, previousHours ->
            let currentDayMeetingHours =
                availableHoursUtc
                |> List.filter (fun x -> x.Days = 0)

            let nextDayMeetingHoursWithoutOffset =
                nextDaysHours
                |> List.map (fun x -> TimeSpan(x.Hours, x.Minutes, x.Seconds))

            let previousDayMeetingHoursWithoutOffset =
                previousHours
                |> List.map (fun x -> TimeSpan(x.Hours, x.Minutes, x.Seconds))

            [ generateMeetingHoursForOffsetDay previousDayMeetingHoursWithoutOffset false
              { WeekDayName = dayOfTheWeekName
                UtcHours = currentDayMeetingHours }
              generateMeetingHoursForOffsetDay nextDayMeetingHoursWithoutOffset true ]

    let extractApplicantSchedule (row: MentorshipInformation.Row) =
        let convertAvailabilityIndexToTimeRange index =
            match index with
            | 0 ->
                Some(
                    availableLocalTimeHoursForMentorship
                    |> List.take 3
                )
            | 1 ->
                Some(
                    availableLocalTimeHoursForMentorship
                    |> List.skip 3
                    |> List.take 3
                )
            | 2 ->
                Some(
                    availableLocalTimeHoursForMentorship
                    |> List.skip 6
                    |> List.take 3
                )
            | 3 ->
                Some(
                    availableLocalTimeHoursForMentorship
                    |> List.skip 9
                    |> List.take 3
                )
            | 4 ->
                Some(
                    availableLocalTimeHoursForMentorship
                    |> List.skip 12
                    |> List.take 3
                )
            | _ -> None

        let convertDateAndTimeToAvailability
            (weekDayAvailability: string)
            (availabilityIndex: int)
            (utcOffsetValue: TimeSpan)
            =
            if String.IsNullOrEmpty weekDayAvailability then
                None
            else
                let optAvailabilityRange =
                    convertAvailabilityIndexToTimeRange availabilityIndex

                match optAvailabilityRange with
                | None -> None
                | Some availabilityRange ->
                    let availableRangeInUtc =
                        availabilityRange
                        |> List.map (fun x -> x.Add(utcOffsetValue))

                    if weekDayAvailability.Contains(';') <> true then
                        (weekDayAvailability, availableRangeInUtc)
                        ||> distributeOffsetHoursToSeparateDays
                        |> Some
                    else
                        weekDayAvailability
                        |> splitStringAndRemoveDelimiters
                        |> List.map (fun weekDayName -> (weekDayName, availableRangeInUtc))
                        |> List.map
                            (fun (weekDay, hoursInUtc) -> distributeOffsetHoursToSeparateDays weekDay hoursInUtc)
                        |> List.concat
                        |> List.groupBy (fun x -> x.WeekDayName)
                        |> List.map
                            (fun (availableDay, availableHours) ->
                                { WeekDayName = availableDay
                                  UtcHours =
                                      availableHours
                                      |> List.map (fun x -> x.UtcHours)
                                      |> List.concat })
                        |> Some

        // The timezone, as mentioned in the CSV data, is in fact a UTC offset, not a proper TZ
        let utcOffset =
            if row.``What is your time zone?``.Equals("UTC") then
                TimeSpan(0, 0, 0)
            else
                let normalizedUtcValue =
                    row
                        .``What is your time zone?``
                        .Replace("UTC", "")
                        .Replace("+", "")
                        .Replace(" ", "")

                TimeSpan(Int32.Parse(normalizedUtcValue), 0, 0)

        let availableDays =
            [ row.``What time are you available? [09:00 - 12:00 local time]``
              row.``What time are you available? [12:00 - 15:00 local time]``
              row.``What time are you available? [15:00 - 18:00 local time]``
              row.``What time are you available? [18:00 - 21:00 local time]``
              row.``What time are you available? [21:00 - 00:00 local time]`` ]
            |> List.mapi (fun idx dateAndTime -> convertDateAndTimeToAvailability dateAndTime idx utcOffset)
            |> List.chooseDefault
            |> List.concat
            |> List.groupBy (fun x -> x.WeekDayName)
            |> List.map
                (fun (weekDayName, dayAvailabilities) ->
                    let utcHours =
                        dayAvailabilities
                        |> List.map (fun availableDay -> availableDay.UtcHours)
                        |> List.concat

                    { WeekDayName = weekDayName
                      UtcHours = utcHours })

        { AvailableDays = NonEmptyList.ofList availableDays }

    let extractApplicantInformation (row: MentorshipInformation.Row) =
        { Fullname = row.``What is your full name (First and Last Name)``
          SlackName = row.``What is your fsharp.org slack name?``
          EmailAddress = row.``Email address``
          MentorshipSchedule = extractApplicantSchedule row }

    let deepDiveInFSharpKeywords =
        [ "Deep";
          "dive";
          "investment";
          "better" ]

    let mobileDevelopmentKeywords =
        [ "Uno";
          "Fabulous";
          "Xamarin";
          "Mobile";
          "Mobile development" ]

    let distributedSystemKeywords =
        [ "Microservices";
          "Distributed systems";
          "event sourcing" ]

    let webDevelopmentKeywords =
        [ "Web";
          "Elmish"; 
          "Fable"; 
          "SAFE"; 
          "Giraffe"; 
          "React"; 
          "Feliz"; 
          "MVC"; 
          "Web development / SAFE stack" ]

    let openSourceKeywords =
        [ "Contribute to an open source project";
          "open source" ]

    let extractFsharpTopic (row: MentorshipInformation.Row) =
        let convertCategoryNameToTopic categoryName =
            let matchOnStringCategory (stringCategory: string) =
                let doesCategoryMatchKeyword (categoryName: string) (keywordList: string list) =
                    keywordList
                    |> List.exists
                        (fun keyword -> categoryName.Contains(keyword, StringComparison.InvariantCultureIgnoreCase))

                let category =
                    if stringCategory.[0] = ' ' then
                        stringCategory.Substring(1)
                    else
                        stringCategory

                if String.Equals("Introduction to F#", category, StringComparison.InvariantCultureIgnoreCase) ||  category.Contains("beginner", StringComparison.InvariantCultureIgnoreCase)  then
                    Some introduction

                elif
                    String.Equals
                        (
                            "Contribute to an open source project",
                            category,
                            StringComparison.InvariantCultureIgnoreCase
                        )
                then
                    Some contributeToOSS

                elif String.Equals("Machine learning", category, StringComparison.InvariantCultureIgnoreCase) then
                    Some machineLearning

                elif String.Equals("Contribute to the compiler", category, StringComparison.InvariantCultureIgnoreCase) then
                    Some contributeToCompiler

                elif String.Equals("Designing with types", category, StringComparison.InvariantCultureIgnoreCase) then
                    Some designingWithTypes
                    
                elif String.Equals("Meta programming", category, StringComparison.InvariantCultureIgnoreCase) then
                    Some metaProgramming

                elif String.Equals("Domain modeling", category, StringComparison.InvariantCultureIgnoreCase) then
                    Some domainModeling

                elif category.Equals("up for anything", StringComparison.InvariantCultureIgnoreCase) || category.Contains("All of the above", StringComparison.InvariantCultureIgnoreCase) then
                    Some upForAnything
                    
                elif doesCategoryMatchKeyword category openSourceKeywords then
                    Some contributeToOSS

                elif doesCategoryMatchKeyword category deepDiveInFSharpKeywords then
                    Some deepDive

                elif doesCategoryMatchKeyword category mobileDevelopmentKeywords then
                    Some mobileDevelopment

                elif doesCategoryMatchKeyword category distributedSystemKeywords then
                    Some distributedSystems

                elif doesCategoryMatchKeyword category webDevelopmentKeywords then
                    Some webDevelopment

                else
                    Some upForAnything

            if String.IsNullOrEmpty categoryName then
                None
            elif categoryName.Contains(',') <> true then
                Some [ matchOnStringCategory categoryName ]
            else
                categoryName.Split(',')
                |> List.ofArray
                |> List.map
                    (fun x ->
                        if x.[0] = ' ' then
                            x.Substring(1)
                        else
                            x)
                |> List.map (fun x -> matchOnStringCategory x)
                |> Some

        if String.IsNullOrEmpty row.``What topic do you want to learn?`` then
            convertCategoryNameToTopic row.``What topics do you feel comfortable mentoring?``
        else
            convertCategoryNameToTopic row.``What topic do you want to learn?``

    let extractPeopleInformation (mentorshipDocument: MentorshipInformation) =
        let extractFsharpTopics (rows: MentorshipInformation.Row seq) =
            rows
            |> List.ofSeq
            |> List.map extractFsharpTopic
            |> List.chooseDefault
            |> List.concat
            |> List.chooseDefault
            |> NonEmptyList.ofList

        let mentors =
            mentorshipDocument.Rows
            |> List.ofSeq
            |> List.filter
                (fun row ->
                    String.IsNullOrWhiteSpace(row.``What topics do you feel comfortable mentoring?``)
                    <> true)
            |> List.groupBy (fun row -> row.``What is your full name (First and Last Name)``)
            |> List.map
                (fun x ->
                    let multipleMentorEntries = snd x
                    let mentorData = List.head multipleMentorEntries

                    { MentorInformation = extractApplicantInformation mentorData
                      SimultaneousMenteeCount =
                          multipleMentorEntries
                          |> Seq.filter
                              (fun x ->
                                  String.IsNullOrEmpty x.``What topics do you feel comfortable mentoring?``
                                  <> true)
                          |> Seq.length
                          |> uint
                      AreasOfExpertise = extractFsharpTopics multipleMentorEntries })

        let mentees =
            mentorshipDocument.Rows
            |> List.ofSeq
            |> List.filter
                (fun row ->
                    String.IsNullOrWhiteSpace(row.``What topic do you want to learn?``)
                    <> true)
            |> List.groupBy (fun row -> row.``What is your full name (First and Last Name)``)
            |> List.map
                (fun (_, multipleMenteeEntries) ->
                    let menteeData = List.head multipleMenteeEntries

                    { MenteeInformation = extractApplicantInformation menteeData
                      TopicsOfInterest = extractFsharpTopics multipleMenteeEntries })

        { Mentors = mentors; Mentees = mentees }

[<RequireQualifiedAccess>]
module CsvExtractor =
    let extractApplicantsInformation (csvDocumentFilePath: string) =
        {   
            Applicants = csvDocumentFilePath |> MentorshipInformation.Load |> Impl.extractPeopleInformation
            ConfirmedMatches = []
            MatchedMenteesSet = Set.empty
            MatchedMentorSet = Set.empty
            NumberOfHoursRequiredForOverlap = 1 }