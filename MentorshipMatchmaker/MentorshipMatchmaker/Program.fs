open System
open System.IO

open Argu

open MentorshipMatchmaker.Domain
open MentorshipMatchmaker.Infra


[<CliPrefix(CliPrefix.DoubleDash)>]
[<NoAppSettings>]
type private CliArgument =
    | CreateMentorshipMatches of csvDocumentPath: string
with
    interface IArgParserTemplate with
        member cliArg.Usage =
            match cliArg with
            | CreateMentorshipMatches _ ->
                "Provide relative path the CSV document containing the current's round information
                with all the applicant's data."

type InputValidationError =
    | InputMissing
    | RelativePathDoesNotExists of string
    | NoMatchPossible of string
with
    member x.ErrorMessage =
        match x with
        | InputMissing ->
            "The provided argument is missing/empty. Please provide a path for the file."
        | RelativePathDoesNotExists relativePath ->
            sprintf "The relative path to the CSV document %A does not exists. Please check your input."
                relativePath
        | NoMatchPossible relativePath ->
            sprintf "The provided file %A couldn't produce a single match between a mentor and a mentee.
            Please consult your data."
                relativePath


[<EntryPoint>]
let main argv =
    // Don't forget to provide the current CSV document for the mentorship.
    // Please leave the CSV document out of the repository. It's been excluded in the git ignore.
    // Don 't commit the file in the repository.
    let run (parsedArguments: ParseResults<CliArgument>) =
        match parsedArguments.GetAllResults() with
        | [] ->
            Error InputMissing

        | toolMode :: _ ->
            match toolMode with
            | CreateMentorshipMatches csvDocumentPath ->
                if String.IsNullOrWhiteSpace csvDocumentPath then
                    Error InputMissing
                elif File.Exists(csvDocumentPath) <> true then
                    Error (RelativePathDoesNotExists csvDocumentPath)
                else
                    let (mentors, mentees) = CsvExtractor.extract csvDocumentPath
                    let optMentorshipMatches = Matchmaking.tryGenerateMentorshipConfirmedApplicantList mentees mentors

                    match optMentorshipMatches with
                    | None ->
                        Error (NoMatchPossible csvDocumentPath)

                    | Some mentorshipMatches ->
                        Ok mentorshipMatches

    let exitColorizer  = function
        | ErrorCode.HelpText -> None
        | _ -> Some ConsoleColor.Red

    let errorHandler = ProcessExiter(colorizer = exitColorizer)
    // Settings checkStructure to false blocks Argu from checking if the DU is properly formed
    // -- avoids performance hit via Reflection.
    let cliArgumentParser = ArgumentParser.Create<CliArgument>(checkStructure = false, errorHandler = errorHandler, programName = "Mentorship Matchmaker")
    let resultFromRunningTool = (cliArgumentParser.ParseCommandLine argv) |> run

    match resultFromRunningTool with
    | Error error ->
        printfn "%A" (error.ErrorMessage)
        -1
    | Ok mentorshipMatches ->
        0
