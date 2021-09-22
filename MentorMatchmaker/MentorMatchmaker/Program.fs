open System
open System.IO

open MentorMatchmaker.DomainOperations
open MentorMatchmaker.Infra
open MentorMatchmaker.EmailGeneration

open Argu

[<CliPrefix(CliPrefix.DoubleDash)>]
[<NoAppSettings>]
type private CliArgument =
    | CreateMentorshipMatches of csvDocumentPath: string
    interface IArgParserTemplate with
        member cliArg.Usage =
            match cliArg with
            | CreateMentorshipMatches _ ->
                "The relative path to the CSV document containing the current round's information with all the applicants' data."

type InputValidationError =
    | InputMissing
    | RelativePathDoesNotExists of string
    | NoMatchPossible of string
    member x.ErrorMessage =
        match x with
        | InputMissing -> "The provided argument is missing/empty. Please provide a path for the file."
        | RelativePathDoesNotExists relativePath ->
            $"The relative path to the CSV document {relativePath} does not exists. Please check your input."
        | NoMatchPossible relativePath ->
            $"The provided file {relativePath} couldn't produce a single match between a mentor and a mentee. Please consult your data."

[<EntryPoint>]
let main argv =
    // Don't forget to provide the current CSV document for the mentorship.
    // Please leave the CSV document out of the repository. It's been excluded in the git ignore.
    // Don 't commit the file in the repository.
    let run (parsedArguments: ParseResults<CliArgument>) =
        match parsedArguments.GetAllResults() with
        | [] -> Error InputMissing

        | toolMode :: tail ->
            match toolMode with
            | CreateMentorshipMatches (csvDocumentPath) ->
                if String.IsNullOrEmpty csvDocumentPath then
                    Error InputMissing
                elif File.Exists(csvDocumentPath) <> true then
                    Error(RelativePathDoesNotExists csvDocumentPath)
                else
                    let mentorshipPairings, plannerInputs =
                        Path.GetFullPath(csvDocumentPath)
                        |> CsvExtractor.extractMentorshipPlannerInputs
                        |> Matchmaking.getMentorshipPairing

                    match mentorshipPairings with
                    | [] -> Error(NoMatchPossible csvDocumentPath)

                    | _ -> Ok(mentorshipPairings, plannerInputs)

    let errorHandler =
        ProcessExiter(
            colorizer =
                function
                | ErrorCode.HelpText -> None
                | _ -> Some System.ConsoleColor.Red
        )

    let cliArgumentParser =
        ArgumentParser.Create<CliArgument>(
            checkStructure = false,
            errorHandler = errorHandler,
            programName = "Mentor matchmaker"
        ) // Settings checkStructure to false blocks Argu from checking if the DU is properly formed -- avoids performance hit via Reflection.

    let resultFromRunningTool =
        (cliArgumentParser.ParseCommandLine argv) |> run

    match resultFromRunningTool with
    | Error error ->
        printfn $"{error.ErrorMessage}"
        -1
    | Ok (mentorshipPairings, plannerInputs) ->
        mentorshipPairings
        |> List.map EmailGenerationService.dumpTemplateEmailsInFile
        |> ignore

        plannerInputs
        |> Matchmaking.dumpToFileUnmatchedApplicants

        0
