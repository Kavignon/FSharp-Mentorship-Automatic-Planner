﻿module MentorMatchmaker.DomainOperations

open System.Linq

open FSharpPlus.Data

open Infra
open Utilities
open DomainTypes

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

let extractMatchingFsharpTopics (mentorFsharpTopics: FsharpTopic nel) (menteeFsharpTopics: FsharpTopic nel) =
    NonEmptyList.intersect mentorFsharpTopics menteeFsharpTopics

let doesMenteeMatchMentorProfile (mentor: Mentor) (mentee: Mentee) =
    let foundScheduleOverlap = (mentee.MenteeInformation.MentorshipSchedule, mentor.MentorInformation.MentorshipSchedule) ||> doScheduleOverlap
    let hasAnyMatchingInterest = (mentor.AreasOfExpertise, mentee.TopicsOfInterest) ||> extractMatchingFsharpTopics |> Seq.length > 0
    
    foundScheduleOverlap && hasAnyMatchingInterest

let findAllMatchingMenteesForMentor (mentor: Mentor) (mentees: Mentee list) =
    mentees 
    |> List.filter(fun mentee -> doesMenteeMatchMentorProfile mentor mentee)
    |> List.map(fun mentee ->  { Mentor = mentor; Mentee = mentee; MatchingFsharpInterests = extractMatchingFsharpTopics mentee.TopicsOfInterest mentor.AreasOfExpertise } )

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
    |> NonEmptyList.ofList

let canMatchMenteesWithMentors listOfMentors listOfMentees =
    listOfMentors
    |> List.exists(fun mentor ->
        (mentor, listOfMentees)
        ||> findAllMatchingMenteesForMentor
        |> fun matches -> List.isNotEmpty matches)

let findAllPotentialMentorshipMatches mentors mentees=
    mentors 
    |> List.map(fun mentor -> findAllMatchingMenteesForMentor mentor mentees)
    |> List.concat
    |> List.map(fun mentorshipMatch -> 
        let orderedInterestBasedOnPopularity = mentorshipMatch.MatchingFsharpInterests |> List.sortByDescending(fun fsharpTopic -> fsharpTopic.PopularityWeight)
        { mentorshipMatch with MatchingFsharpInterests = orderedInterestBasedOnPopularity }
    )

let rec createUniqueMentorshipMatches (matches: Map<Mentor, ConfirmedMentorshipApplication list>) (matchedMentees: Set<Mentee>) (potentialMatches: PotentialMentorshipMatch list) =
    let prepareDataForMatching (potentialMatch: PotentialMentorshipMatch) (confirmedMatches: ConfirmedMentorshipApplication list) (potentialMatchesRemaining: PotentialMentorshipMatch list) =
        let mentee = potentialMatch.Mentee
        let mentor = potentialMatch.Mentor
        let confirmedmentorshipMatch = {
            Mentee = mentee
            Mentor = mentor
            FsharpTopic = potentialMatch.MatchingFsharpInterests.Head
            MeetingTimes = generateMeetingTimes mentee.MenteeInformation.MentorshipSchedule mentor.MentorInformation.MentorshipSchedule
        }

        let updatedMappings = matches |> Map.add mentor (confirmedmentorshipMatch :: confirmedMatches)
        let updatedMatchedMentees = matchedMentees |> Set.add mentee

        (updatedMappings, updatedMatchedMentees, potentialMatchesRemaining)

    match potentialMatches with
    | [] -> matches
    | potentialMatch :: remainingPotentialMatches ->
        if matchedMentees.Contains potentialMatch.Mentee then
            createUniqueMentorshipMatches matches matchedMentees remainingPotentialMatches
        else
            let optMentorWithMatches = matches |> Map.tryFind potentialMatch.Mentor
            match optMentorWithMatches with
            | Some confirmedMatches when confirmedMatches.Length >= (potentialMatch.Mentor.SimultaneousMenteeCount |> int) ->
                createUniqueMentorshipMatches matches matchedMentees remainingPotentialMatches
            
            | Some confirmedMatches ->
                (potentialMatch, confirmedMatches, remainingPotentialMatches) 
                |||> prepareDataForMatching
                |||> createUniqueMentorshipMatches

            | None ->
                (potentialMatch, [], remainingPotentialMatches) 
                |||> prepareDataForMatching
                |||> createUniqueMentorshipMatches

[<RequireQualifiedAccess>]
module Matchmaking =
    let tryGenerateMentorshipConfirmedApplicantList (mentees: Mentee list) (mentors: Mentor list) =
        let atLeastOneMatchPossible = canMatchMenteesWithMentors mentors mentees
        if atLeastOneMatchPossible <> true then
            None
        else
            (mentors, mentees)
            ||> findAllPotentialMentorshipMatches
            |> createUniqueMentorshipMatches Map.empty Set.empty
            |> Map.map(fun _ confirmedMatches -> confirmedMatches)
            |> Map.toList
            |> Some