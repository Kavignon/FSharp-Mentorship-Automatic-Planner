module MentorMatchmaker.DomainOperations

open System.Linq

open FSharpPlus.Data

open Infra
open Utilities
open DomainTypes
open EmailGeneration
open System.Net.Mail

let checkForAvailabilityMatch mentorAvailability menteeAvailability hoursOfOverlap =
    let anyCommonSlot =
        List.intersect menteeAvailability.UtcHours mentorAvailability.UtcHours
        |> List.length
        >= hoursOfOverlap

    mentorAvailability.WeekDayName.Equals(menteeAvailability.WeekDayName)
    && anyCommonSlot

let doScheduleOverlap menteeSchedule mentorSchedule hoursOfOverlap =
    let menteeAvailabilities =
        menteeSchedule.AvailableDays
        |> NonEmptyList.toList

    let mentorAvailabilities =
        mentorSchedule.AvailableDays
        |> NonEmptyList.toList

    let anySlotBetweenApplicants menteeSchedule hoursOfOverlap =
        List.exists
            (fun mentorSchedule -> checkForAvailabilityMatch menteeSchedule mentorSchedule hoursOfOverlap)
            mentorAvailabilities

    List.exists (fun menteeSchedule -> anySlotBetweenApplicants menteeSchedule hoursOfOverlap) menteeAvailabilities

let extractMatchingInterestTopics (mentorInterestTopics: InterestTopic nel) (menteeInterestTopics: InterestTopic nel) =
    NonEmptyList.intersect mentorInterestTopics menteeInterestTopics

let doesMenteeMatchMentorProfile (mentor: Mentor) (mentee: Mentee) (hoursOfOverlap: int) =
    let foundScheduleOverlap =
         doScheduleOverlap mentee.MenteeInformation.MentorshipSchedule mentor.MentorInformation.MentorshipSchedule hoursOfOverlap

    let hasAnyMatchingInterest =
        (mentor.AreasOfExpertise, mentee.TopicsOfInterest)
        ||> extractMatchingInterestTopics
        |> Seq.length > 0

    foundScheduleOverlap && hasAnyMatchingInterest

let findAllMatchingMenteesForMentor (mentees: Mentee list) (hoursOfOverlap: int) (mentor: Mentor)  =
    mentees
    |> List.filter (fun mentee -> doesMenteeMatchMentorProfile mentor mentee hoursOfOverlap)
    |> List.map
        (fun mentee ->
            { Mentor = mentor
              Mentee = mentee
              MatchingFsharpInterests = extractMatchingInterestTopics mentee.TopicsOfInterest mentor.AreasOfExpertise })

let tryFindSameAvailableHoursForApplicants menteeAvailableDay mentorAvailableDay =
    let sameAvailableHours =
        List.intersect menteeAvailableDay.UtcHours mentorAvailableDay.UtcHours
        |> List.toConsecutivePairs
        |> List.filter (fun (previousHour, currentHour) -> previousHour.Hours + 1 = currentHour.Hours)
        |> List.map
            (fun (previousHour, currentHour) ->
                { UtcStartTime = previousHour
                  UtcEndTime = currentHour })

    if sameAvailableHours.Length = 0 then
        None
    else
        Some
            { Weekday = menteeAvailableDay.WeekDayName
              MatchedAvailablePeriods = NonEmptyList.create sameAvailableHours.Head sameAvailableHours.Tail }
let filterForSameWeekDay (availableDays1: DayAvailability nel) (availableDays2: DayAvailability nel) =
    let availabilitiesList1 = availableDays1 |> NonEmptyList.toList
    let availabilitiesList2 = availableDays2 |> NonEmptyList.toList
    let filterDay day = availabilitiesList2 |> List.exists(fun x -> x.WeekDayName = day.WeekDayName)
    availabilitiesList1 |> List.filter filterDay


// See also 'generateMeetingTimes2'
let generateMeetingTimes (mentorSchedule: CalendarSchedule) (menteeSchedule: CalendarSchedule) =

    let sortByWeekDayName nonEmptyAvailableDays =
        nonEmptyAvailableDays
        |> NonEmptyList.toList
        |> List.sortBy (fun x -> x.WeekDayName)

    let mentorAvailableDaysList, menteeAvailableDaysList =
        if mentorSchedule.AvailableDays.Length = menteeSchedule.AvailableDays.Length then
            sortByWeekDayName mentorSchedule.AvailableDays, sortByWeekDayName menteeSchedule.AvailableDays
        else
            let mentorWeekSchedule = filterForSameWeekDay  mentorSchedule.AvailableDays  menteeSchedule.AvailableDays  
            let menteeWeekSchedule =  filterForSameWeekDay menteeSchedule.AvailableDays  mentorSchedule.AvailableDays
            sortByWeekDayName (NonEmptyList.ofList mentorWeekSchedule), sortByWeekDayName (NonEmptyList.ofList menteeWeekSchedule) // List.Head and List.Tail are not safe (throws on empty list)

    (mentorAvailableDaysList, menteeAvailableDaysList)
    ||> List.map2 tryFindSameAvailableHoursForApplicants
    |> List.chooseDefault




let canMatchMenteesWithMentors listOfMentors listOfMentees hoursOfOverlap =
    let matches = (findAllMatchingMenteesForMentor listOfMentees hoursOfOverlap) >> List.isNotEmpty
    listOfMentors |> List.exists matches

let findAllPotentialMentorshipMatches mentors mentees hoursOfOverlap =
    mentors
    |> List.map (findAllMatchingMenteesForMentor mentees hoursOfOverlap)
    |> List.concat
    |> List.map
        (fun mentorshipMatch ->
            let orderedInterestBasedOnPopularity =
                mentorshipMatch.MatchingFsharpInterests
                |> List.sortByDescending (fun topic -> topic.PopularityWeight)

            { mentorshipMatch with
                  MatchingFsharpInterests = orderedInterestBasedOnPopularity })

type CollectingMentorshipPairing =
    { Matches: Map<Mentor, ConfirmedMentorshipApplication list>
      MatchedMentees: Set<Mentee>
      MatchedMentors: Set<Mentor>
      RemainingPotentialMatches: PotentialMentorshipMatch list }

let rec createUniqueMentorshipMatches (collectingPairing: CollectingMentorshipPairing) =
    let prepareDataForMatching
        (potentialMatch: PotentialMentorshipMatch)
        (confirmedMatches: ConfirmedMentorshipApplication list)
        (potentialMatchesRemaining: PotentialMentorshipMatch list)
        =
        let mentee = potentialMatch.Mentee
        let mentor = potentialMatch.Mentor

        let sessionHours =
            generateMeetingTimes mentee.MenteeInformation.MentorshipSchedule mentor.MentorInformation.MentorshipSchedule

        if sessionHours.Length > 0 then
            let confirmedMentorshipMatch =
                { MatchedMentee = mentee
                  MatchedMentor = mentor
                  InterestTopics = potentialMatch.MatchingFsharpInterests
                  CouldMentorHandleMoreWork = (confirmedMatches.Length + 1 = mentor.SimultaneousMenteeCount)
                  MeetingTimes = NonEmptyList.create sessionHours.Head sessionHours.Tail }

            let updatedCollectionPairing =
                { Matches =
                      collectingPairing.Matches
                      |> Map.add mentor (confirmedMentorshipMatch :: confirmedMatches)
                  MatchedMentees = collectingPairing.MatchedMentees |> Set.add mentee
                  MatchedMentors = collectingPairing.MatchedMentors |> Set.add mentor
                  RemainingPotentialMatches =
                      potentialMatchesRemaining
                      |> List.filter (fun x -> x <> potentialMatch) }

            updatedCollectionPairing

        else
            { collectingPairing with
                  RemainingPotentialMatches = potentialMatchesRemaining.Tail }

    match collectingPairing.RemainingPotentialMatches with
    | [] -> collectingPairing

    | potentialMatch :: remainingPotentialMatches ->
        if collectingPairing.MatchedMentees.Contains potentialMatch.Mentee
           || collectingPairing.MatchedMentors.Contains potentialMatch.Mentor then
            createUniqueMentorshipMatches
                { collectingPairing with
                      RemainingPotentialMatches = remainingPotentialMatches }
        else
            let optMentorWithMatches =
                collectingPairing.Matches
                |> Map.tryFind potentialMatch.Mentor
            let simultaneousMenteeCount = potentialMatch.Mentor.SimultaneousMenteeCount 
            match optMentorWithMatches with
            | Some confirmedMatches when confirmedMatches.Length >= simultaneousMenteeCount ->
                createUniqueMentorshipMatches collectingPairing
            | Some confirmedMatches ->
                prepareDataForMatching potentialMatch confirmedMatches collectingPairing.RemainingPotentialMatches
                |> createUniqueMentorshipMatches
            | None ->
                prepareDataForMatching potentialMatch [] collectingPairing.RemainingPotentialMatches
                |> createUniqueMentorshipMatches

let getConfirmedMatchesFromPlanner (plannerInputs: MentorshipPlannerInputs) =
    let collectingPairings =
        let mentors = plannerInputs.Applicants.Mentors
        let mentees = plannerInputs.Applicants.Mentees
        let hoursRequired = plannerInputs.NumberOfHoursRequiredForOverlap
        { Matches = Map.empty
          MatchedMentees = plannerInputs.MatchedMenteesSet
          MatchedMentors = plannerInputs.MatchedMentorSet
          RemainingPotentialMatches = findAllPotentialMentorshipMatches  mentors mentees hoursRequired}
        |> createUniqueMentorshipMatches

    let mentorshipPairings =
        collectingPairings.Matches
        |> Map.map (fun _ confirmedMatches -> confirmedMatches)
        |> Map.toList
        |> List.map (fun (_, confirmedMatches) -> confirmedMatches)
        |> List.concat

    (mentorshipPairings, collectingPairings.MatchedMentees, collectingPairings.MatchedMentors)

[<RequireQualifiedAccess>]
module Matchmaking =
    // TODO : Get auth data from somewhere
    let createSmtpClient () =
        let client = new SmtpClient(@"smtp.gmail.com")
    
        client.UseDefaultCredentials <- false
        client.EnableSsl <- true
        client.Port <- 587
        client.Credentials <- System.Net.NetworkCredential("mentorship@fsharp.org", "") // TODO : Password/credentials should be retrieved .gitignored settings file
        client.DeliveryMethod <- SmtpDeliveryMethod.Network
    
        client

    // TODO: Could be fun to improve the backend and domain model by discerning between a unmatched and matched applicant. Plus removing this bool would make me feel a lot better.
    type UnmatchedApplicant =
        {   Name: string
            EmailAddress: string
            IsMentor: bool
            Interests: InterestTopic nel
            AvailableDays: DayAvailability nel }

    let generateUnmatchedApplicantsDump unmatchedApplicants =
        let dumpToFileApplicationData (unmatchedApplicant: UnmatchedApplicant) =
            let topics =
                unmatchedApplicant.Interests
                |> NonEmptyList.map (fun topic -> topic.Name)
                |> String.concat " ,"

            let availabilities =
                unmatchedApplicant.AvailableDays
                |> NonEmptyList.map
                    (fun day ->
                        let availableHours =
                            day.UtcHours
                            |> List.map (fun utc -> $"{utc.Hours}")
                            |> String.concat " ,"

                        $"{day.WeekDayName}: {availableHours}")
                |> String.concat ", "

            let applicantType =
                if unmatchedApplicant.IsMentor then
                    "Mentor"
                else
                    "Mentee"

            $"
                {applicantType}: Name -> {unmatchedApplicant.Name} Email -> {unmatchedApplicant.EmailAddress}
                Topics: {topics}
                Available hours in UTC:
                        {availabilities}
            "

        unmatchedApplicants 
        |> List.map(fun application -> $"{dumpToFileApplicationData application}")
        |> String.concat("\n")

    let createPlannerInputs (applicants: Applicants) =
        {   Applicants = applicants
            ConfirmedMatches = []
            MatchedMenteesSet = Set.empty
            MatchedMentorSet = Set.empty
            NumberOfHoursRequiredForOverlap = 1 }

    let rec getMentorshipPairing (plannerInputs: MentorshipPlannerInputs) =
        let filterToUnmatchedMentees (unmatchedMentees: Mentee list) (matchedMentees: Set<Mentee>) =
            unmatchedMentees |> List.filter(fun mentee -> matchedMentees.Contains mentee <> true)

        let filterToUnmatchedMentors (unmatchedMentors: Mentor list) (confirmedPairings: ConfirmedMentorshipApplication list) =
            unmatchedMentors |> List.filter(fun unmatchedMentor -> confirmedPairings.Any(fun x -> x.MatchedMentor = unmatchedMentor) <> true)

        match (plannerInputs.Applicants.Mentors, plannerInputs.NumberOfHoursRequiredForOverlap) with
        | ([], _) ->
            plannerInputs.ConfirmedMatches, plannerInputs

        | (_, 0) ->
            plannerInputs.ConfirmedMatches, plannerInputs

        | _ ->
            let (confirmedMatches, matchedMenteeSet, matchedMentorsSet) = getConfirmedMatchesFromPlanner plannerInputs
            let updatedPlanner = 
                { plannerInputs with
                    Applicants = {
                        Mentees = filterToUnmatchedMentees plannerInputs.Applicants.Mentees plannerInputs.MatchedMenteesSet
                        Mentors = filterToUnmatchedMentors plannerInputs.Applicants.Mentors plannerInputs.ConfirmedMatches
                    }
                    ConfirmedMatches = plannerInputs.ConfirmedMatches @ confirmedMatches
                    MatchedMenteesSet = matchedMenteeSet
                    MatchedMentorSet = matchedMentorsSet
                    NumberOfHoursRequiredForOverlap = plannerInputs.NumberOfHoursRequiredForOverlap - 1 }

            getMentorshipPairing updatedPlanner
    
    let createUnmatchedApplicantPairingPermutations (mentees: UnmatchedApplicant list) (mentors: UnmatchedApplicant list) =
        let matchPermutations = 
            [
                for mentor in mentors do
                    let menteeMatches = 
                        [
                            for mentee in mentees do
                                let sessionHours = generateMeetingTimes { CalendarSchedule.AvailableDays = mentee.AvailableDays } { CalendarSchedule.AvailableDays = mentor.AvailableDays }
                                if mentor.EmailAddress <> mentee.EmailAddress && sessionHours.Any() then
                                    Some {| Mentor = mentor; Mentee = mentee; SessionHours = sessionHours |}
                                else
                                    None
                        ]
                        |> List.choose id

                    {| Mentor = mentor; Mentees = menteeMatches |}
            ]

        matchPermutations

    let unmatchedApplicants (plannerInputs: MentorshipPlannerInputs) = 
        { Mentees = plannerInputs.Applicants.Mentees |> List.filter(fun mentee -> plannerInputs.MatchedMenteesSet.Contains mentee <> true)
          Mentors = plannerInputs.Applicants.Mentors |> List.filter(fun mentor -> plannerInputs.MatchedMentorSet.Contains mentor <> true) }

    let mapToUnmatchedApplicants (applicants: Applicants) =
        let transformedMenteesInUnmatchedApplicants = 
            applicants.Mentees
            |> List.map(fun x -> 
                { 
                    Name = x.MenteeInformation.Fullname
                    EmailAddress = x.MenteeInformation.EmailAddress
                    IsMentor = false
                    Interests = x.TopicsOfInterest
                    AvailableDays = x.MenteeInformation.MentorshipSchedule.AvailableDays
                })

        let transformedMentorsInUnmatchedApplicants = 
            applicants.Mentors
            |> List.map(fun x -> 
                { 
                    Name = x.MentorInformation.Fullname
                    EmailAddress = x.MentorInformation.EmailAddress
                    IsMentor = true
                    Interests = x.AreasOfExpertise
                    AvailableDays = x.MentorInformation.MentorshipSchedule.AvailableDays
                })

        transformedMenteesInUnmatchedApplicants, transformedMentorsInUnmatchedApplicants
        
    let generateUnmatchedApplicantsDumpFileContents (applicants: Applicants): string =
        let transformedMenteesInUnmatchedApplicants, transformedMentorsInUnmatchedApplicants = mapToUnmatchedApplicants applicants
        let unmatchedApplicants = transformedMenteesInUnmatchedApplicants @ transformedMentorsInUnmatchedApplicants
        let fileContent = generateUnmatchedApplicantsDump unmatchedApplicants
        fileContent

    let dumpMeetingTimes (meetingTimes: OverlapSchedule nel) =
        meetingTimes
        |> NonEmptyList.map(fun meetingDay ->
            let aggregatedTimes = meetingDay.MatchedAvailablePeriods |> NonEmptyList.toList |> List.fold(fun accumulatedTimes currentTime -> accumulatedTimes + $", {currentTime.UtcStartTime}") ""
            let aggregatedTimes = aggregatedTimes.Substring(2)
            $"{meetingDay.Weekday}: {aggregatedTimes}"
        )
        |> String.concat " ;"


    let generateUnmatchedApplicantPairingPermutationsFileContents (applicants: Applicants): string =
        let transformedMenteesInUnmatchedApplicants, transformedMentorsInUnmatchedApplicants = mapToUnmatchedApplicants applicants
               
        let unmatchedApplicantsPairings = createUnmatchedApplicantPairingPermutations transformedMenteesInUnmatchedApplicants transformedMentorsInUnmatchedApplicants

        let renderMentorPairingPermutations (permutations: {| Mentees: {| Mentee: UnmatchedApplicant; Mentor: UnmatchedApplicant; SessionHours: OverlapSchedule list |} list; Mentor: UnmatchedApplicant |}) =
            let renderTopics applicant = 
                let interests = applicant.Interests |> NonEmptyList.map(fun x -> x.Category.CategoryName) |> String.concat(", ")
                $"Topics: {interests}\n"

                       
            let renderMentee (mentee: UnmatchedApplicant) (hours: OverlapSchedule list) =
                let menteeData = $"\n    Mentee: {mentee.Name} ({mentee.EmailAddress})\n"
                let menteeInterests = "    " + renderTopics mentee
                let sessionHours = "        " + dumpMeetingTimes (NonEmptyList.ofList hours)

                $"\n{menteeData}{menteeInterests}{sessionHours}"

            let divider = "\n------------------------------------------------\n\n\n"
            let header = $"Mentor: {permutations.Mentor.Name} ({permutations.Mentor.EmailAddress})\n"
            let mentorTopics = renderTopics permutations.Mentor
            let menteeData = permutations.Mentees |> List.map (fun x -> renderMentee x.Mentee x.SessionHours) |> String.concat ", "

            $"{divider}{header}{mentorTopics}{menteeData}"
                       
        let pairingPermutationsFileContent =
            unmatchedApplicantsPairings
            |> List.map renderMentorPairingPermutations
            |> String.concat ("\n")

        pairingPermutationsFileContent

    let dumpToFileUnmatchedApplicants (plannerInputs: MentorshipPlannerInputs) =

        let unmatched = unmatchedApplicants plannerInputs

        (* Dump unmatched applications *)
        let fileContent = generateUnmatchedApplicantsDumpFileContents unmatched

        ("applicationDataDumpUnmatchedApplications.txt", fileContent) |> System.IO.File.WriteAllText

        (* Dump unmatched applications -- Pairing permutations *)
        let pairingPermutationsFileContent = generateUnmatchedApplicantPairingPermutationsFileContents unmatched
        
        ("applicationDataDumpUnmatchedApplicationsPairingPermutations.txt", pairingPermutationsFileContent) |> System.IO.File.WriteAllText


    (* Service *)

    let convertToJson csvDocumentPath =
        csvDocumentPath
        |> CsvExtractor.extractApplicantsInformation
        |> FileManager.saveVersionedJson "applicants"


    let createMatches applicantsJsonPath =
        match FileManager.readJson<Applicants> applicantsJsonPath with
        | Ok applicants ->
            // Perform the matching, output the matches json
            let mentorshipPairings, plannerInputs =
                applicants
                |> createPlannerInputs
                |> getMentorshipPairing

            let unmatched = unmatchedApplicants plannerInputs

            let outputFileNameMatches = FileManager.saveVersionedJson "matches" mentorshipPairings
            let outputFileNameUnmatches = FileManager.saveVersionedJson "unmatched" unmatched

            Ok {| matches = outputFileNameMatches; unmatched = outputFileNameUnmatches |}

        | Error err ->
            printfn $"Error: {err}"
            Error err

    let matchesToCsv matchesJsonPath =
        match FileManager.readJson<ConfirmedMentorshipApplication list> matchesJsonPath with
        | Ok matches ->
            let fileContents =
                [
                    [ "Mentor"; "Mentee"; "Topic"; "Mentor could handle more" ]
                    for pair in matches do
                        [
                            pair.MatchedMentor.MentorInformation.Fullname + $" ({pair.MatchedMentor.MentorInformation.EmailAddress}) ({pair.MatchedMentor.MentorInformation.SlackName})"
                            pair.MatchedMentee.MenteeInformation.Fullname + $" ({pair.MatchedMentee.MenteeInformation.EmailAddress}) ({pair.MatchedMentee.MenteeInformation.SlackName})"
                            pair.InterestTopics |> List.map (fun topic -> topic.Category.CategoryName) |> List.sort |> String.concat "," 
                            pair.CouldMentorHandleMoreWork.ToString()
                            dumpMeetingTimes pair.MeetingTimes
                        ]
                ]
                |> List.map (String.concat ";")
                |> String.concat "\n"

            let outputFileNameMatchesCsv = FileManager.saveVersionedCsv "matches" fileContents

            Ok outputFileNameMatchesCsv

        | Error err ->
            printfn $"Error: {err}"
            Error err


    let unmatchedDataDump applicantsJsonPath =
        match FileManager.readJson<Applicants> applicantsJsonPath with
        | Ok applicants ->
            let fileContents = applicants |> generateUnmatchedApplicantsDumpFileContents

            let outputFileName = FileManager.saveVersionedText "applicationDataDumpUnmatchedApplications" fileContents

            Ok outputFileName

        | Error err ->
            printfn $"Error: {err}"
            Error err

    let unmatchedPermutationsDataDump applicantsJsonPath =
        match FileManager.readJson<Applicants> applicantsJsonPath with
        | Ok applicants ->
            let fileContents = applicants |> generateUnmatchedApplicantPairingPermutationsFileContents

            let outputFileName = FileManager.saveVersionedText "applicationDataDumpUnmatchedApplicationsPairingPermutations" fileContents
            
            Ok outputFileName

        | Error err ->
            printfn $"Error: {err}"
            Error err

    let generateExampleEmails matchesJsonPath = 
        match FileManager.readJson<ConfirmedMentorshipApplication list> matchesJsonPath with
        | Ok matches ->
            let fileContents = matches |> EmailGenerationService.generateEmailExamplesForMatches

            let outputFileName = FileManager.saveVersionedText "exampleEmailsDump" fileContents

            Ok outputFileName

        | Error err ->
            printfn $"Error: {err}"
            Error err

    
    let sendEmailsMatched matchesJsonPath =
        match FileManager.readJson<ConfirmedMentorshipApplication list> matchesJsonPath with
        | Ok matches ->
            use client = createSmtpClient ()

            for pair in matches do
                let mails = EmailGenerationService.generateEmailsForMatch pair

                use menteeMailMessage =
                    new MailMessage(
                        "mentorship@fsharp.org",
                        pair.MatchedMentee.MenteeInformation.EmailAddress + "," + pair.MatchedMentor.MentorInformation.EmailAddress,
                        @"FSSF Mentorship Program: Congratulations and meet your mentorship partner",
                        mails.MenteeEmail)

                menteeMailMessage.IsBodyHtml <- true

                client.Send menteeMailMessage

                use mentorMailMessage = 
                    new MailMessage(
                        "mentorship@fsharp.org",
                        pair.MatchedMentor.MentorInformation.EmailAddress,
                        @"FSSF Mentorship Program: Get started as a mentor",
                        mails.MentorEmail)
                        
                mentorMailMessage.IsBodyHtml <- true

                client.Send mentorMailMessage

            Ok ()
    
        | Error err ->
            printfn $"Error: {err}"
            Error err

    let concatenateEmailsUnmatched applicantsJsonPath =
        match FileManager.readJson<Applicants> applicantsJsonPath with
        | Ok applicants ->
            let fileContents = applicants.Mentees |> List.map (fun x -> x.MenteeInformation.EmailAddress) |> String.concat ";"
            let ouputFileName = FileManager.saveVersionedText "unmatchedEmails" fileContents

            Ok ouputFileName
        | Error err ->
            printfn $"Error: {err}"
            Error err






