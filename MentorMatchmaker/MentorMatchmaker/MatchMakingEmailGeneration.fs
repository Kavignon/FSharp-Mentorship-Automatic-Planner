module MentorMatchmaker.EmailGeneration

open MentorMatchmaker.Utilities
open MentorMatchmaker.DomainTypes

module private Implementation =
    type MentorEmailTemplateInformation =
        {   MentorFirstName: string
            LengthOfMentorshipInWeeks: int
            MenteeFirstName: string
            Email: string
            FssfSlack: string
            FsharpTopic: FsharpTopic
            MenteeAvailabilitiesInUtc: OverlapSchedule nel }

    type MenteeEmailTemplateInformation =
        {   MenteeFirstName: string
            MentorFirstName: string
            LengthOfMentorshipInWeeks: int
            }

    type MentorshipEmailTemplate =
        | Mentor of MentorEmailTemplateInformation
        | Mentee of MenteeEmailTemplateInformation

    type Email =
        {   From: string
            To: string
            BodyOfText: string
            ReplyTo: string
            Subject: string }

    let dumpMatchingToFile (confirmedApplications: ConfirmedMentorshipApplication list) =
        let dumpMeetingTimes (meetingTimes: OverlapSchedule nel) =
            meetingTimes
            |> NonEmptyList.map(fun meetingDay ->
                let aggregatedTimes = meetingDay.MatchedAvailablePeriods |> NonEmptyList.toList |> List.fold(fun accumulatedTimes currentTime -> accumulatedTimes + $", {currentTime.UtcStartTime}") ""
                let aggregatedTimes = aggregatedTimes.Substring(2)
                $"\n {meetingDay.Weekday}: {aggregatedTimes}"
            )
            |> String.concat("\t\t\t")
    
        let dumpToFileApplicationData (application: ConfirmedMentorshipApplication) =
            $"
                Mentor: Name -> {application.Mentor.MentorInformation.Fullname} Email -> {application.Mentor.MentorInformation.EmailAddress}
                Could have supported more students: {application.CouldMentorHandleMoreWork}
                Max simultaneous students possible: {application.Mentor.SimultaneousMenteeCount}
                Mentee: Name -> {application.Mentee.MenteeInformation.Fullname} Email -> {application.Mentee.MenteeInformation.EmailAddress}
                Topic: {application.FsharpTopic.Name}
                Possible meeting hours (in UTC): {dumpMeetingTimes application.MeetingTimes}
            "

        let fileContent = 
            confirmedApplications 
            |> List.map(fun application -> $"{dumpToFileApplicationData application}")
            |> String.concat("\n")
    
        System.IO.File.WriteAllText("applicationDataDump.txt", fileContent)

    let transformConfirmedMatchIntoMenteeEmailTemplate (confirmedMatch: ConfirmedMentorshipApplication) =
        Mentee {
        }

    let transformConfirmedMatchIntoMentorEmailTemplate (confirmedMatch: ConfirmedMentorshipApplication) =
        Mentor {
        }

    let transformMentorshipEmailTemplateIntoEmail (mentorshipEmailTemplate: MentorshipEmailTemplate) : Email =
        {
        }

    let formatEmailIntoString (email: Email) : string =
        ""

[<RequireQualifiedAccess>]
module EmailGenerationService =
    open Implementation

    let dumpTemplateEmailsInFile (mentorshipMatch: ConfirmedMentorshipApplication) =
        let fileContent =
            [ transformConfirmedMatchIntoMenteeEmailTemplate mentorshipMatch; transformConfirmedMatchIntoMentorEmailTemplate mentorshipMatch]
            |> List.map transformMentorshipEmailTemplateIntoEmail
            |> List.map formatEmailIntoString
            |> String.concat("\n")

        System.IO.File.WriteAllText("templateEmailToSendDump.txt", fileContent)