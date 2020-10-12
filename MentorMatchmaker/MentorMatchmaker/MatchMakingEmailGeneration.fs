module MatchMaking.EmailGeneration

open MentorMatchmaker.Utilities
open MentorMatchmaker.DomainTypes

module private Implementation =
    type MentorEmailTemplateToken =
        {   MentorFirstName: string
            LengthOfMentorshipInWeeks: int
            MenteeFirstName: string
            MentorEmail: string
            FssfSlack: string
            FsharpTopic: FsharpTopic
            MenteeAvailabilitiesInUtc: OverlapSchedule nel }

    type MenteeEmailTemplateToken =
        {   MenteeFirstName: string
            MentorFirstName: string
            LengthOfMentorshipInWeeks: int
            MenteeEmailAddress: string
            }

    type MentorshipEmailTemplateToken =
        | Mentor of MentorEmailTemplateToken
        | Mentee of MenteeEmailTemplateToken

    let transformIntoMenteeTokens (confirmedMatch: ConfirmedMentorshipApplication) =
        Mentee {
            MenteeFirstName = confirmedMatch.Mentee.MenteeInformation.FirstName
            MentorFirstName = confirmedMatch.Mentor.MentorInformation.FirstName
            MenteeEmailAddress = confirmedMatch.Mentee.MenteeInformation.EmailAddress
            LengthOfMentorshipInWeeks = 8 // TODO: I need a a way to retrieve it and to make it not static....
        }

    let transformIntoMentorTokens (confirmedMatch: ConfirmedMentorshipApplication) =
        Mentor {
            MentorFirstName = confirmedMatch.Mentor.MentorInformation.FirstName
            LengthOfMentorshipInWeeks = 8
            MenteeFirstName = confirmedMatch.Mentee.MenteeInformation.FirstName
            MentorEmail = confirmedMatch.Mentor.MentorInformation.EmailAddress
            FssfSlack = confirmedMatch.Mentor.MentorInformation.SlackName
            FsharpTopic = confirmedMatch.FsharpTopic
            MenteeAvailabilitiesInUtc = confirmedMatch.MeetingTimes
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