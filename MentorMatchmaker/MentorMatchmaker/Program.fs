open MentorMatchmaker.Domain
open MentorMatchmaker.Infra


[<EntryPoint>]
let main _ =
    // Don't forget to provide the current CSV document for the mentorship.
    // Please leave the CSV document out of the repository. It's been excluded in the git ignore.
    // Don 't commit the file in the repository.
    let (mentors, mentees) = CsvExtractor.extract()
    let matches = Matchmaking.tryGenerateMentorshipConfirmedApplicantList mentees mentors
    Matchmaking.matchMenteesWithMentors [] mentees mentors
    |> printfn "%A"
    ////|> generateOrganizationEmail
    ////|> outputMailToLinkInHtml

    0 // return an integer exit code
