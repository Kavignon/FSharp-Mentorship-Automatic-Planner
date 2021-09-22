open System
open System.IO

open MentorMatchmaker.DomainTypes
open MentorMatchmaker.DomainOperations
open MentorMatchmaker.Infra
open MentorMatchmaker.EmailGeneration

open Argu

[<CliPrefix(CliPrefix.DoubleDash)>]
[<NoAppSettings>]
type private CliArgument =
    | [<CustomCommandLine("csv");AltCommandLine("-c")>] CreateMentorshipMatches of csvDocumentPath: string
    | [<CustomCommandLine("tzdiff");AltCommandLine("-t")>] MaxTimezoneDifference of maxTimezoneDifference: int
with
    interface IArgParserTemplate with
        member cliArg.Usage =
            match cliArg with
            | CreateMentorshipMatches _ -> 
                "The relative path to the CSV document containing the current's round information with all the applicants' data."
            | MaxTimezoneDifference  _ -> 
                "The upper bound of the allowable timezone differential between two participants."

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
    let configure (args:ParseResults<_>)=
        let rec inner cfg args =
            match args with
            | [] -> cfg
            | head::tail ->
                match head with
                | CreateMentorshipMatches csvDocumentPath ->
                    inner (Some csvDocumentPath,snd cfg) tail
                | MaxTimezoneDifference i ->
                    inner (fst cfg, Some i) tail

        match inner (None,None) (args.GetAllResults()) with
        | (Some csv, Some timezone) -> Ok {CSVPath = csv; MaxTimezoneDifference = timezone}
        | (Some csv, None)          -> Ok {CSVPath = csv; MaxTimezoneDifference = 10}
        | (None, Some _)            -> Error InputMissing
        | (None,None)               -> Error InputMissing

    // Don't forget to provide the current CSV document for the mentorship.
    // Please leave the CSV document out of the repository. It's been excluded in the git ignore.
    // Don 't commit the file in the repository.
    let run config =
        if String.IsNullOrEmpty config.CSVPath then
            Error InputMissing
        elif File.Exists(config.CSVPath) <> true then
            Error (RelativePathDoesNotExists config.CSVPath)
        else
            let mentorshipPairings, plannerInputs =
                config
                |> CsvExtractor.extractMentorshipPlannerInputs
                |> Matchmaking.getMentorshipPairing
                
            match mentorshipPairings with
            | [] ->
                Error (NoMatchPossible config.CSVPath)
        
            | _ ->
                Ok (mentorshipPairings, plannerInputs)      

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
        (cliArgumentParser.ParseCommandLine argv) 
        |> configure 
        |> Result.bind run

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
