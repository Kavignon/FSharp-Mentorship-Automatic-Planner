module MentorMatchmaker.EmailGeneration

open FSharpPlus.Data

open MentorMatchmaker.Utilities
open MentorMatchmaker.DomainTypes

module private Implementation =
    type MentorEmailTemplateToken =
        { MentorFirstName: string
          LengthOfMentorshipInWeeks: int
          MenteeFirstName: string
          MentorEmail: string
          FssfSlack: string
          FsharpTopic: FsharpTopic }

    type MenteeAndMentorPairTemplateTokens =
        { MenteeFirstName: string
          MenteeFssfSlack: string
          MentorFirstName: string
          MentorFssfSlack: string
          LengthOfMentorshipInWeeks: int
          MentorshipInterest: FsharpTopic
          MenteeEmailAddress: string
          MentorEmailAddress: string
          AvailableMeetingSessionsInUtc: OverlapSchedule nel }

    type MentorshipEmailTemplateToken =
        | Mentor of MentorEmailTemplateToken
        | MenteeAndMentorPair of MenteeAndMentorPairTemplateTokens

    let transformIntoMenteeTokens (confirmedMatch: ConfirmedMentorshipApplication) =
        MenteeAndMentorPair
            { MentorshipInterest = confirmedMatch.FsharpTopic
              MenteeFirstName = confirmedMatch.MatchedMentee.MenteeInformation.FirstName
              MenteeFssfSlack = confirmedMatch.MatchedMentee.MenteeInformation.SlackName
              MentorFirstName = confirmedMatch.MatchedMentor.MentorInformation.FirstName
              MentorFssfSlack = confirmedMatch.MatchedMentor.MentorInformation.SlackName
              MenteeEmailAddress = confirmedMatch.MatchedMentee.MenteeInformation.EmailAddress
              MentorEmailAddress = confirmedMatch.MatchedMentor.MentorInformation.EmailAddress
              LengthOfMentorshipInWeeks = 8 // TODO: I need a a way to retrieve it and to make it not static....
              AvailableMeetingSessionsInUtc = confirmedMatch.MeetingTimes }

    let transformIntoMentorTokens (confirmedMatch: ConfirmedMentorshipApplication) =
        Mentor
            { MentorFirstName = confirmedMatch.MatchedMentor.MentorInformation.FirstName
              LengthOfMentorshipInWeeks = 8
              MenteeFirstName = confirmedMatch.MatchedMentee.MenteeInformation.FirstName
              MentorEmail = confirmedMatch.MatchedMentor.MentorInformation.EmailAddress
              FssfSlack = confirmedMatch.MatchedMentor.MentorInformation.SlackName
              FsharpTopic = confirmedMatch.FsharpTopic }

    let dumpMeetingTimes (meetingTimes: OverlapSchedule nel) =
        meetingTimes
        |> NonEmptyList.map(fun meetingDay ->
            let aggregatedTimes =
                meetingDay.MatchedAvailablePeriods
                |> NonEmptyList.toList
                |> List.fold(fun accumulatedTimes currentTime -> accumulatedTimes + $", {currentTime.UtcStartTime}") ""
                |> fun times -> times.Substring(2)
                
            $"{meetingDay.Weekday}: {aggregatedTimes}<br>"
        )
        |> String.concat("\t\t\t")

    let replaceMenteeTemplateWithTokens (menteeTokens: MenteeAndMentorPairTemplateTokens) =
        $"
Hello {menteeTokens.MentorFirstName} and {menteeTokens.MenteeFirstName},<br><br>
 
Congratulations! You have been selected to participate in this round of the F# Software Foundation’s Mentorship Program.<br><br>

We have paired the two of you together because we noticed you’re both interested in {menteeTokens.MentorshipInterest.Name} and that your availability matched. With that said, we are hoping for great things!<br><br>

As part of this round of mentorship, we’d recommend that you meet with your other half for at least one hour per week over the next {menteeTokens.LengthOfMentorshipInWeeks} weeks. You can choose your prefered way of getting together virtually, whether that being via Skype, Google Hangout, Slack, etc. As a mentorship pair, the individual arrangements are left up to you to sort out.<br><br>

Please reach out to us via this email or education@fsharp.org if you have any questions or concerns.<br><br>

Feel free to take it from here.<br><br>

<b>Mentee Details</b><br><br>

Mentee FSSF Slack username: {menteeTokens.MenteeFssfSlack}<br>
Mentor FSSF Slack username: {menteeTokens.MentorFssfSlack}<br><br>

<b>Possible meeting sessions (in UTC)</b><br><br>
{dumpMeetingTimes menteeTokens.AvailableMeetingSessionsInUtc}<br><br>

Thank you, and happy learning!<br><br>

F# Software Foundation Mentorship Program<br><br>

Mentor email: {menteeTokens.MentorEmailAddress}<br><br>
Mentee email: {menteeTokens.MenteeEmailAddress}<br><br>
        "

    let replaceMentorTemplateWithTokens (mentorTokens: MentorEmailTemplateToken) =
        $"
Hello {mentorTokens.MentorFirstName},<br><br>

Thank you so much for volunteering to be a mentor!<br><br>

You should have received an e-mail pairing you with your mentee, here we just want to provide you with a few helpful instructions:<br><br>

1. It is the responsibility of the mentor (you) to make first contact<br><br>

2. Suggest a first meeting: set a date/time (that is why you have your mentees matches on your schedule and their time zone) for the first meeting, in which you can plan the mentorship.<br><br>

3. We’d recommend that you meet with your mentee for at least one hour per week over the next 8 weeks.<br><br>

4.  Mentors will receive an invite to the private mentor-only slack channel. Feel free to chat about the mentorship experience with your fellow mentors here. Ask anything, whether it’s<br><br>

for additional learning resources that you would like to share with your learner, or for help with a question that  your learner asked that you’re not sure how to answer, or even to chat about the mentorship experience overall.<br><br>

5. If your mentee has not responded within three days, or if your schedules don't match after all, please send us an email (or ping us in the mentor channel on slack). This way we can set you up with a replacement.<br><br>

<<<<<<< HEAD
6. To ease facilitation for us and being able to help early, please add \"mentorship@fsharp.org\" in CC of your first e-mail or press \"reply all\" to our introduction email which you should have received.<br><br>
=======
            6. To ease facilitation for us and being able to help early, please add \"mentorship@fsharp.org\" in CC of your first e-mail or press \"reply all\" to our introduction email which will follow shortly.

            Mentor email: {mentorTokens.MentorEmail}
>>>>>>> 831e05a7724886985c3e1157c3acf9d8935a4dd3
        "

    let formatEmailToSend (mentorshipEmailTemplateTokens: MentorshipEmailTemplateToken) : string =
        match mentorshipEmailTemplateTokens with
        | MenteeAndMentorPair menteeTokens -> replaceMenteeTemplateWithTokens menteeTokens
        | Mentor mentorTokens -> replaceMentorTemplateWithTokens mentorTokens

[<RequireQualifiedAccess>]
module EmailGenerationService =
    open Implementation

    let generateEmailsForMatch (mentorshipMatch: ConfirmedMentorshipApplication) =
        {|
            MenteeEmail = transformIntoMenteeTokens mentorshipMatch |> formatEmailToSend
            MentorEmail = transformIntoMentorTokens mentorshipMatch |> formatEmailToSend
        |}


    let generateEmailExamplesForMatch (mentorshipMatch: ConfirmedMentorshipApplication) =
        let fileContent =
            [ transformIntoMenteeTokens mentorshipMatch
              transformIntoMentorTokens mentorshipMatch ]
            |> List.map formatEmailToSend
            |> String.concat ("\n")

        fileContent

    let generateEmailExamplesForMatches (matches: ConfirmedMentorshipApplication list) =
        matches |> List.map generateEmailExamplesForMatch |> String.concat "/n"

    let dumpTemplateEmailsInFile (mentorshipMatch: ConfirmedMentorshipApplication) =
        let fileContent = generateEmailExamplesForMatch mentorshipMatch

        System.IO.File.AppendAllText("templateEmailToSendDump.txt", fileContent + System.Environment.NewLine)
