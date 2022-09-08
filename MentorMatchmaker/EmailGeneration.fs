
[<RequireQualifiedAccess>]
module MentorMatchmaker.EmailGeneration

open MentorMatchmaker
open System.Net.Mail

[<AutoOpen>]
module private Implementation =
    let LengthOfMentorshipInWeeks = 8 // TODO: I need a a way to retrieve it and to make it not static....

    let rec topicName topic =
        match topic with
        | IntroductionToFSharp -> "Introduction to F#"
        | DeepDiveInFSharp -> "Deep dive in F#"
        | ContributeToOpenSource -> "Contribute to open-source"
        | ContributeToCompiler -> "Contribute to compiler"
        | WebDevelopment -> "Web development"
        | DistributedSystems -> "Distributed systems"
        | DomainModelling -> "Domain modeling"
        | MobileDevelopment -> "Mobile development"
        | MachineLearning -> "Machine learning"
        | UpForAnything -> "I am up for anything"
        | DesigningWithTypes -> "Designing with types"
        | MetaProgramming -> "Meta programming"

    let printLocalMeetingTimes offset (meetingTimes: Set<WeekTime>) =
        meetingTimes
        |> Set.map (fun time -> time.AddHours(offset))
        |> toWeekTimeRanges
        |> Seq.groupBy (fun weekTimeRange -> weekTimeRange.Start.Weekday)
        |> Seq.map (fun (weekday, weekTimeRanges) ->
            let aggregatedTimes =
                weekTimeRanges
                |> Seq.map (fun range -> range.Start.Time.ToShortTimeString() + "-" + range.End.Time.AddHours(1).ToShortTimeString())
                |> String.concat ", "

            $"{weekday}: {aggregatedTimes}<br>"
        )
        |> String.concat("\t\t\t")

    let formatListOfSharedInterests topics =
        topics
        |> Seq.map topicName
        |> String.concat ", "

    let possessive (name:string) = name + (if name[name.Length - 1] = 's' then "'" else "'s")

    let generateMenteeMentorEmail { Mentor = mentor; Mentee = mentee; MutualTopics = topics; MutualAvailabilities = availabilities } =
        let mentor = mentor.PersonalInformation
        let mentee = mentee.PersonalInformation
        $"
Hello {mentor.FirstName} and {mentee.FirstName},<br><br>

Congratulations! You have been selected to participate in this round of the F# Software Foundation’s Mentorship Program.<br><br>

We have paired the two of you together because we noticed you’re both interested in {formatListOfSharedInterests topics} and that your availability matched. With that said, we are hoping for great things!

Please reach out to us via this email or education@fsharp.org if you have any questions or concerns.<br><br>

Feel free to take it from here.<br><br>

<b>Mentee Details</b><br><br>

Mentee FSSF Slack username: {mentee.SlackName}<br>
Mentor FSSF Slack username: {mentor.SlackName}<br><br>

<b>Possible meeting sessions ({possessive mentee.FirstName} local time)</b><br><br>
{printLocalMeetingTimes mentee.LocalOffset availabilities}<br><br>

<b>Possible meeting sessions ({possessive mentor.FirstName} local time)</b><br><br>
{printLocalMeetingTimes mentor.LocalOffset availabilities}<br><br>

Thank you, and happy learning!<br><br>

F# Software Foundation Mentorship Program<br><br>

Mentor email: {mentor.EmailAddress}<br><br>
Mentee email: {mentee.EmailAddress}<br><br>
"

    let generateMentorEmail {Mentor = mentor} =
        let mentor = mentor.PersonalInformation
        $"
Hello {mentor.FirstName},<br><br>

Thank you so much for volunteering to be a mentor!<br><br>

You should have received an e-mail pairing you with your mentee, here we just want to provide you with a few helpful instructions:<br><br>

1. It is the responsibility of the mentor (you) to make first contact<br><br>

2. Suggest a first meeting: set a date/time (that is why you have your mentees matches on your schedule and their time zone) for the first meeting, in which you can plan the mentorship.<br><br>

3. We’d recommend that you meet with your mentee for at least one hour per week over the next {LengthOfMentorshipInWeeks} weeks.<br><br>

4.  Mentors will receive an invite to the private mentor-only slack channel. Feel free to chat about the mentorship experience with your fellow mentors here. Ask anything, whether it’s<br><br>

for additional learning resources that you would like to share with your learner, or for help with a question that  your learner asked that you’re not sure how to answer, or even to chat about the mentorship experience overall.<br><br>

5. If your mentee has not responded within three days, or if your schedules don't match after all, please send us an email (or ping us in the mentor channel on slack). This way we can set you up with a replacement.<br><br>

6. To ease facilitation for us and being able to help early, please add \"mentorship@fsharp.org\" in CC of your first e-mail or press \"reply all\" to our introduction email which will follow shortly.

Mentor email: {mentor.EmailAddress}
"

let sendEmailToPairedApplicants (smtpClient: SmtpClient) (mentorshipPair: MentorshipPair) =
    let mentor = mentorshipPair.Mentor.PersonalInformation
    let mentee = mentorshipPair.Mentee.PersonalInformation
    use menteeMailMessage =
        new MailMessage(
            from = "mentorship@fsharp.org",
            ``to`` = mentee.EmailAddress + "," + mentor.EmailAddress,
            subject = @"FSSF Mentorship Program: Congratulations and meet your mentorship partner",
            body = generateMenteeMentorEmail mentorshipPair,
            IsBodyHtml = true)

    smtpClient.Send menteeMailMessage

    use mentorMailMessage =
        new MailMessage(
            from = "mentorship@fsharp.org",
            ``to`` = mentor.EmailAddress,
            subject = @"FSSF Mentorship Program: Get started as a mentor",
            body = generateMentorEmail mentorshipPair,
            IsBodyHtml = true)

    smtpClient.Send mentorMailMessage

let generateEmailExamples (matches: MentorshipPair list) =
    matches
    |> List.collect (fun mentorshipPair ->
        [ generateMenteeMentorEmail mentorshipPair
          generateMentorEmail mentorshipPair ])
    |> String.concat "\n"

let createSmtpClient (password:string) =
    new SmtpClient(
        host = @"smtp.gmail.com",
        UseDefaultCredentials = false,
        EnableSsl = true,
        Port = 587,
        Credentials = System.Net.NetworkCredential("mentorship@fsharp.org", password),
        DeliveryMethod = SmtpDeliveryMethod.Network)
