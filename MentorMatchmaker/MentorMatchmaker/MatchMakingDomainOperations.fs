module MentorMatchmaker.DomainOperations

open System.Linq

open FSharpPlus.Data

open Infra
open Utilities
open DomainTypes

let checkForAvailabilityMatch mentorAvailability menteeAvailability hoursOfOverlap =
    let anyCommonSlot =
        List.intersect menteeAvailability.UtcHours mentorAvailability.UtcHours
        |> List.length >= hoursOfOverlap
    mentorAvailability.WeekDayName.Equals(menteeAvailability.WeekDayName) && anyCommonSlot

let doScheduleOverlap menteeSchedule mentorSchedule hoursOfOverlap=
    let menteeAvailabilities = menteeSchedule.AvailableDays |> NonEmptyList.toList
    let mentorAvailabilities = mentorSchedule.AvailableDays |> NonEmptyList.toList
    
    let anySlotBetweenApplicants menteeSchedule hoursOfOverlap=
        List.exists(fun mentorSchedule -> checkForAvailabilityMatch menteeSchedule mentorSchedule hoursOfOverlap) mentorAvailabilities

    List.exists(fun menteeSchedule -> anySlotBetweenApplicants menteeSchedule hoursOfOverlap) menteeAvailabilities

let extractMatchingFsharpTopics (mentorFsharpTopics: FsharpTopic nel) (menteeFsharpTopics: FsharpTopic nel) =
    NonEmptyList.intersect mentorFsharpTopics menteeFsharpTopics

let doesMenteeMatchMentorProfile (mentor: Mentor) (mentee: Mentee) (hoursOfOverlap: int)=
    let foundScheduleOverlap = (mentee.MenteeInformation.MentorshipSchedule, mentor.MentorInformation.MentorshipSchedule, hoursOfOverlap) |||> doScheduleOverlap
    let hasAnyMatchingInterest = (mentor.AreasOfExpertise, mentee.TopicsOfInterest) ||> extractMatchingFsharpTopics |> Seq.length > 0
    
    foundScheduleOverlap && hasAnyMatchingInterest

let findAllMatchingMenteesForMentor (mentor: Mentor) (mentees: Mentee list) (hoursOfOverlap: int) =
    mentees 
    |> List.filter(fun mentee -> doesMenteeMatchMentorProfile mentor mentee hoursOfOverlap)
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
        Some { Weekday = menteeAvailableDay.WeekDayName; MatchedAvailablePeriods = NonEmptyList.create sameAvailableHours.Head sameAvailableHours.Tail }

let generateMeetingTimes (mentorSchedule: CalendarSchedule) (menteeSchedule: CalendarSchedule) =
    let filterForSameWeekDay (availableDays1: DayAvailability nel) (availableDays2: DayAvailability nel) =
        let availabilitiesList1 = availableDays1 |> NonEmptyList.toList
        let availabilitiesList2 = availableDays2 |> NonEmptyList.toList

        availabilitiesList1 |> List.filter(fun day -> availabilitiesList2 |> List.exists(fun x -> x.WeekDayName = day.WeekDayName))

    let sortByWeekDayName nonEmptyAvailableDays =
        nonEmptyAvailableDays
        |> NonEmptyList.toList
        |> List.sortBy(fun x -> x.WeekDayName)

    let mentorAvailableDaysList, menteeAvailableDaysList =
        if mentorSchedule.AvailableDays.Length = menteeSchedule.AvailableDays.Length then
            sortByWeekDayName mentorSchedule.AvailableDays, sortByWeekDayName menteeSchedule.AvailableDays
        else
            let mentorWeekSchedule = (mentorSchedule.AvailableDays, menteeSchedule.AvailableDays) ||> filterForSameWeekDay
            let menteeWeekSchedule = (menteeSchedule.AvailableDays, mentorSchedule.AvailableDays) ||> filterForSameWeekDay
            sortByWeekDayName (NonEmptyList.create mentorWeekSchedule.Head mentorWeekSchedule.Tail), sortByWeekDayName (NonEmptyList.create menteeWeekSchedule.Head menteeWeekSchedule.Tail)

    (mentorAvailableDaysList, menteeAvailableDaysList)
    ||> List.map2(fun mentorAvailabilitiesOfTheDay menteeAvailabilitiesOfTheDay -> tryFindSameAvailableHoursForApplicants mentorAvailabilitiesOfTheDay menteeAvailabilitiesOfTheDay)
    |> List.chooseDefault

let canMatchMenteesWithMentors listOfMentors listOfMentees hoursOfOverlap=
    listOfMentors
    |> List.exists(fun mentor ->
        (mentor, listOfMentees, hoursOfOverlap)
        |||> findAllMatchingMenteesForMentor
        |> fun matches -> List.isNotEmpty matches)

let findAllPotentialMentorshipMatches mentors mentees hoursOfOverlap =
    mentors 
    |> List.map(fun mentor -> findAllMatchingMenteesForMentor mentor mentees hoursOfOverlap)
    |> List.concat
    |> List.map(fun mentorshipMatch -> 
        let orderedInterestBasedOnPopularity = mentorshipMatch.MatchingFsharpInterests |> List.sortByDescending(fun fsharpTopic -> fsharpTopic.PopularityWeight)
        { mentorshipMatch with MatchingFsharpInterests = orderedInterestBasedOnPopularity }
    )

let rec createUniqueMentorshipMatches (matches: Map<Mentor, ConfirmedMentorshipApplication list>) (matchedMentees: Set<Mentee>) (potentialMatches: PotentialMentorshipMatch list) =
    let prepareDataForMatching (potentialMatch: PotentialMentorshipMatch) (confirmedMatches: ConfirmedMentorshipApplication list) (potentialMatchesRemaining: PotentialMentorshipMatch list) =
        let mentee = potentialMatch.Mentee
        let mentor = potentialMatch.Mentor
        let sessionHours = generateMeetingTimes mentee.MenteeInformation.MentorshipSchedule mentor.MentorInformation.MentorshipSchedule
        if sessionHours.Length > 0 then
            let confirmedMentoshipMatch = {
                MatchedMentee = mentee
                MatchedMentor = mentor
                FsharpTopic = potentialMatch.MatchingFsharpInterests.Head
                CouldMentorHandleMoreWork = confirmedMatches.Length + 1 = (mentor.SimultaneousMenteeCount |> int)
                MeetingTimes = NonEmptyList.create sessionHours.Head sessionHours.Tail
            }

            let updatedMappings = matches |> Map.add mentor (confirmedMentoshipMatch :: confirmedMatches)
            let updatedMatchedMentees = matchedMentees |> Set.add mentee

            (updatedMappings, updatedMatchedMentees, potentialMatchesRemaining)

        else
            (matches, matchedMentees, potentialMatchesRemaining)

    match potentialMatches with
    | [] -> (matches, matchedMentees)
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

let getConfirmedMatchesFromPlanner (plannerInputs: MentorshipPlannerInputs) =
    let (mentorshipPairingsMap, matchedMentees) = 
        (plannerInputs.UnmatchedMentors, plannerInputs.UnmatchedMentees, plannerInputs.NumberOfHoursRequiredForOverlap)
        |||> findAllPotentialMentorshipMatches
        |> createUniqueMentorshipMatches Map.empty plannerInputs.MatchedMenteesSet
    
    let mentorshipPairings =
        mentorshipPairingsMap
        |> Map.map(fun _ confirmedMatches -> confirmedMatches)
        |> Map.toList
        |> List.map(fun (_, confirmedMatches) -> confirmedMatches)
        |> List.concat

    (mentorshipPairings, matchedMentees)

[<RequireQualifiedAccess>]
module Matchmaking =
    open System.IO

    let rec getMentorshipPairing (plannerInputs: MentorshipPlannerInputs) =
        let filterToUnmatchedMentees (unmatchedMentees: Mentee list) (matchedMentees: Set<Mentee>) =
            unmatchedMentees |> List.filter(fun mentee -> matchedMentees.Contains mentee <> true)

        let filterToUnmatchedMentors (unmatchedMentors: Mentor list) (confirmedPairings: ConfirmedMentorshipApplication list) =
            unmatchedMentors |> List.filter(fun unmatchedMentor -> confirmedPairings |> List.exists(fun pairing -> pairing.MatchedMentor = unmatchedMentor) <> true)

        match (plannerInputs.UnmatchedMentors, plannerInputs.NumberOfHoursRequiredForOverlap) with
        | ([], _) -> 
            plannerInputs.ConfirmedMatches

        | (_, 0) ->
            plannerInputs.ConfirmedMatches

        | _ ->
            let (confirmedMatches, matchedMenteeSet) = getConfirmedMatchesFromPlanner plannerInputs
            let updatedPlanner = 
                { plannerInputs with
                    UnmatchedMentees = filterToUnmatchedMentees plannerInputs.UnmatchedMentees plannerInputs.MatchedMenteesSet
                    UnmatchedMentors = filterToUnmatchedMentors plannerInputs.UnmatchedMentors plannerInputs.ConfirmedMatches
                    ConfirmedMatches = plannerInputs.ConfirmedMatches @ confirmedMatches
                    MatchedMenteesSet = matchedMenteeSet
                    NumberOfHoursRequiredForOverlap = plannerInputs.NumberOfHoursRequiredForOverlap - 1 }

            getMentorshipPairing updatedPlanner

