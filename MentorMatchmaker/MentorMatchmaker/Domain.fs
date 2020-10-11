module MentorMatchmaker.Domain

open System
open System.Linq

open FSharpPlus.Data

open Infra
open Utilities


let checkForAvailabilityMatch mentorAvailability menteeAvailability =
    let anyCommonSlot =
        List.intersect menteeAvailability.UtcHours mentorAvailability.UtcHours
        |> List.length >= 2
    mentorAvailability.WeekDayName.Equals(menteeAvailability.WeekDayName) && anyCommonSlot

let findTimeZone utcOffset =
    TimeZoneInfo.GetSystemTimeZones()
    |> Seq.filter(fun x -> x.BaseUtcOffset = utcOffset && x.SupportsDaylightSavingTime = true)
    |> Seq.head // Flaw of implementation and in the data. We only have UTC offset instead of time zones.

let doScheduleOverlap menteeSchedule mentorSchedule =
    let menteeAvailabilities = menteeSchedule.AvailableDays |> NonEmptyList.toList
    let mentorAvailabilities = mentorSchedule.AvailableDays |> NonEmptyList.toList
    let anySlotBetweenApplicants menteeSchedule =
        List.exists(fun mentorSchedule -> checkForAvailabilityMatch menteeSchedule mentorSchedule) mentorAvailabilities

    List.exists(fun menteeSchedule -> anySlotBetweenApplicants menteeSchedule) menteeAvailabilities

let findMatchingMenteeForMentor (mentor: Mentor) (mentees: Mentee list) =
    let fromRarestToCommonExpertiseAreas =
        mentor.AreasOfExpertise
        |> NonEmptyList.toList
        |> List.sortByDescending(fun x -> x.PopularityWeight)

    fromRarestToCommonExpertiseAreas
    |> List.map(fun expertiseArea ->
        mentees |> List.map(fun mentee ->
        let foundScheduleOverlap = (mentee.MenteeInformation.MentorshipSchedule, mentor.MentorInformation.MentorshipSchedule) ||> doScheduleOverlap
        let foundMatchingMentee = foundScheduleOverlap && mentee.TopicsOfInterest.Contains expertiseArea

        if foundMatchingMentee
        then Some (expertiseArea, mentee)
        else None)
    )
    |> List.concat
    |> List.chooseDefault
    |> List.sortByDescending(fun (topic, _) -> topic.PopularityWeight)

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
    |> List.chooseDefault

let findMentorMatchingMenteeInterest listOfMentors listOfMentees =
    listOfMentors
    |> List.map(fun mentor ->
        let menteeMatches = (mentor, listOfMentees) ||> findMatchingMenteeForMentor
        if menteeMatches.Length = 0 then None
        else
            let fsharpTopicAndMenteeTuple = menteeMatches.Head
            let matchedMentee = snd fsharpTopicAndMenteeTuple
            let fsharpTopic = fst fsharpTopicAndMenteeTuple
            let matchingSchedule =  generateMeetingTimes matchedMentee.MenteeInformation.MentorshipSchedule mentor.MentorInformation.MentorshipSchedule
            Some
                { Mentee = matchedMentee
                  Mentor = mentor
                  FsharpTopic = fsharpTopic
                  MeetingTimes = NonEmptyList.ofList matchingSchedule }
    )
    |> List.chooseDefault

let canMatchMenteesWithMentors listOfMentors listOfMentees =
    listOfMentors
    |> List.exists(fun mentor ->
        (mentor, listOfMentees)
        ||> findMatchingMenteeForMentor
        |> fun menteeMatches -> List.isNotEmpty menteeMatches)

[<RequireQualifiedAccess>]
module Matchmaking =
    let rec matchMenteeToMentor
        (matches: ConfirmedMentorshipApplication list)
        (mentees: Mentee list)
        (mentors: Mentor list)
        : ConfirmedMentorshipApplication list =
        match(mentees, mentors) with
        | ([], _) -> matches
        | (_, []) -> matches
        | (listOfMentees, listOfMentors) ->
            let atLeastOneMatchPossible = canMatchMenteesWithMentors listOfMentors listOfMentees
            if atLeastOneMatchPossible <> true then
                matches
            else
                let mentorshipMatches = findMentorMatchingMenteeInterest listOfMentors listOfMentees

                let getCurrentApplicants applicants predicate =
                    applicants
                    |> List.filter(fun x -> mentorshipMatches |> List.exists (fun y -> predicate x y))

                let currentMentors = getCurrentApplicants listOfMentors (fun x y -> x <> y.Mentor)
                let currentMentees = getCurrentApplicants listOfMentees (fun x y -> x <> y.Mentee)

                matchMenteeToMentor (matches @ mentorshipMatches) currentMentees currentMentors
