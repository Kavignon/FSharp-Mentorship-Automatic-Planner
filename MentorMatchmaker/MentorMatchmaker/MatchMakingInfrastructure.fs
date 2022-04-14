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
        stringInput.Split(',')
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
            elif (not isCheckingForNextDay)
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
        | ([], []) ->
            [ { WeekDayName = dayOfTheWeekName
                UtcHours = availableHoursUtc } ]

        | (nextDayHours, []) when nextDayHours.Length > 0 ->
            let nextDayMeetingHoursWithoutOffset =
                nextDayHours
                |> List.map (fun x -> TimeSpan(x.Hours, x.Minutes, x.Seconds))

            let currentDayMeetingHours =
                availableHoursUtc
                |> List.filter (fun x -> x.Days = 0)

            [ { WeekDayName = dayOfTheWeekName
                UtcHours = currentDayMeetingHours }
              generateMeetingHoursForOffsetDay nextDayMeetingHoursWithoutOffset true ]

        | ([], previousDayHours) when previousDayHours.Length > 0 ->
            let previousDayMeetingHoursWithoutOffset =
                previousDayHours
                |> List.map (fun x -> TimeSpan(x.Hours, x.Minutes, x.Seconds))

            let currentDayMeetingHours =
                availableHoursUtc
                |> List.filter (fun x -> x.Days = 0)

            [ generateMeetingHoursForOffsetDay previousDayMeetingHoursWithoutOffset false
              { WeekDayName = dayOfTheWeekName
                UtcHours = currentDayMeetingHours } ]

        | (nextDaysHours, previousHours) ->
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
            if [0..4] |> List.contains(index)  then
                Some (availableLocalTimeHoursForMentorship |> List.skip (index * 3) |> List.take 3)
            else
                None
        let convertDateAndTimeToAvailability (weekDayAvailability: string) (availabilityIndex: int) =
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
                        |> List.map (fun x -> x.Add(utcOffset))

                    if weekDayAvailability.Contains(',') <> true then
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
      

        let availableDays =
            [ row.``What time are you available? [09:00 - 12:00 local time]``
              row.``What time are you available? [12:00 - 15:00 local time]``
              row.``What time are you available? [15:00 - 18:00 local time]``
              row.``What time are you available? [18:00 - 21:00 local time]``
              row.``What time are you available? [21:00 - 00:00 local time]`` ]
            |> List.mapi (fun idx dateAndTime -> convertDateAndTimeToAvailability dateAndTime idx)
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

    
    let extractInterestTopic (row: MentorshipInformation.Row) =
        let convertCategoryNameToTopic (categoryName: string)   =
            if String.IsNullOrEmpty categoryName then
                []
            else
                categoryName.Split(',')
                |> List.ofArray 
                |> List.map (InterestCategory.ofString >> InterestTopic.ofCategory)
                
        if String.IsNullOrEmpty row.``What topic do you want to learn?`` then
            convertCategoryNameToTopic row.``What topics do you feel comfortable mentoring?``
        else
            convertCategoryNameToTopic row.``What topic do you want to learn?``

    let extractPeopleInformation (mentorshipDocument: MentorshipInformation) =
        let extractInterestTopics (rows: MentorshipInformation.Row seq) =
            rows
            |> List.ofSeq
            |> List.map extractInterestTopic
            |> List.concat
            |> NonEmptyList.ofList
        let isText =   String.IsNullOrWhiteSpace >> not
        let mentors =
            mentorshipDocument.Rows
            |> List.ofSeq
            |> List.filter (fun row -> isText (row.``What topics do you feel comfortable mentoring?``))
            |> List.groupBy (fun row -> row.``What is your full name (First and Last Name)``)
            |> List.map
                (fun x ->
                    let multipleMentorEntries = snd x
                    let mentorData = List.head multipleMentorEntries

                    { MentorInformation = extractApplicantInformation mentorData
                      SimultaneousMenteeCount =
                          multipleMentorEntries
                          |> Seq.filter (fun row -> isText (row.``What topics do you feel comfortable mentoring?``))
                          |> Seq.length
                      AreasOfExpertise = extractInterestTopics multipleMentorEntries })

        let mentees =
            mentorshipDocument.Rows
            |> List.ofSeq
            |> List.filter (fun row -> isText row.``What topic do you want to learn?`` )
            |> List.groupBy (fun row -> row.``What is your full name (First and Last Name)``)
            |> List.map
                (fun (_, multipleMenteeEntries) ->
                    let menteeData = List.head multipleMenteeEntries

                    { MenteeInformation = extractApplicantInformation menteeData
                      TopicsOfInterest = extractInterestTopics multipleMenteeEntries })

        { Mentors = mentors; Mentees = mentees }

[<RequireQualifiedAccess>]
module CsvExtractor =
    let extractApplicantsInformation (csvDocumentFilePath: string) =
        MentorshipInformation.Load csvDocumentFilePath |> Impl.extractPeopleInformation
