module MentorshipMatchmaker.Domain

open System.Linq

open FSharpPlus.Data

open Infra
open Utilities

let checkForAvailabilityMatch mentorAvailability menteeAvailability =
    let anyCommonSlot =
        List.intersect menteeAvailability.UtcHours mentorAvailability.UtcHours
        |> List.length >= 3
    mentorAvailability.WeekDayName.Equals(menteeAvailability.WeekDayName) && anyCommonSlot

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

let tryFindSameAvailableHoursForApplicants menteeAvailableDay mentorAvailableDay =
    let sameAvailableHours =
        List.intersect menteeAvailableDay.UtcHours mentorAvailableDay.UtcHours
        |> List.toConsecutivePairs
        |> List.filter(fun (previousHour, currentHour) -> previousHour.Hours + 1 = currentHour.Hours)
        |> List.map(fun (previousHour, currentHour) -> { UtcStartTime = previousHour; UtcEndTime = currentHour })

    if sameAvailableHours.Length = 0 then
        None
    else
        Some { Weekday = menteeAvailableDay.WeekDayName; MatchPeriods = NonEmptyList.create sameAvailableHours.Head sameAvailableHours.Tail }

let generateMeetingTimes (mentorSchedule: CalendarSchedule) (menteeSchedule: CalendarSchedule) =
    let sortByWeekDayName nonEmptyAvailableDays =
        nonEmptyAvailableDays
        |> NonEmptyList.toList
        |> List.sortBy(fun x -> x.WeekDayName)

    let mentorAvailableDaysList = sortByWeekDayName mentorSchedule.AvailableDays
    let menteeAvailableDaysList = sortByWeekDayName menteeSchedule.AvailableDays
    let tryToGenerateOverlappingSchedule mentorAvailableDay = List.map(fun menteeAvailableDay -> tryFindSameAvailableHoursForApplicants mentorAvailableDay menteeAvailableDay) menteeAvailableDaysList

    mentorAvailableDaysList
    |> List.map(fun mentorAvailableDay -> tryToGenerateOverlappingSchedule mentorAvailableDay)
    |> List.concat
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

let tryFindMatchingMenteeForMentorExpertise mentor mentees =
    let menteeMatches = (mentor, mentees) ||> findMatchingMenteeForMentor
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

let canMatchMenteesWithMentors listOfMentors listOfMentees =
    listOfMentors
    |> List.exists(fun mentor ->
        (mentor, listOfMentees)
        ||> findMatchingMenteeForMentor
        |> fun menteeMatches -> List.isNotEmpty menteeMatches)

[<RequireQualifiedAccess>]
module Matchmaker =
    let matchApplicants (mentees: Mentee list) (mentors: Mentor list) =
        let atLeastOneMatchPossible = canMatchMenteesWithMentors mentors mentees
        if atLeastOneMatchPossible <> true then
            None
        else
            mentors
            |> List.map(fun mentor -> tryFindMatchingMenteeForMentorExpertise mentor mentees)
            |> List.chooseDefault
            |> Some

    let rec matchMenteesWithMentors
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
                let getCurrentApplicants applicants predicate = List.filter(fun x -> mentorshipMatches |> List.exists (fun y -> predicate x y)) applicants
                let currentMentors = getCurrentApplicants listOfMentors (fun mentor confirmedApplicant -> mentor.MentorInformation.Fullname = confirmedApplicant.Mentor.MentorInformation.Fullname)
                let currentMentees = getCurrentApplicants listOfMentees (fun mentee confirmedApplicant -> mentee.MenteeInformation.Fullname = confirmedApplicant.Mentee.MenteeInformation.Fullname)

                matchMenteesWithMentors (matches @ mentorshipMatches) currentMentees currentMentors
