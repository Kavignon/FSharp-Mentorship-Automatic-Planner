module MentorshipMatchmaker.AppHandlers

open MentorshipMatchmaker.Domain
open MentorshipMatchmaker.Infra


type ValidPath = ValidPath of string

type MatchmakingError =
    | MatchmakingError

[<RequireQualifiedAccess>]
module ClientHandlers =
    let handle (ValidPath csvDocumentPath) =
        let (mentors, mentees) = CsvExtractor.extract csvDocumentPath
        let applicantsMatches = Matchmaker.matchApplicants mentees mentors
        match applicantsMatches with
        | None -> Error MatchmakingError
        | Some mentorshipMatches -> Ok mentorshipMatches
