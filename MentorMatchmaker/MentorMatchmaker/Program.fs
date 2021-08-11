open System
open System.IO

open MentorMatchmaker.DomainOperations
open MentorMatchmaker.Infra
open MentorMatchmaker.EmailGeneration

open Argu
open MentorMatchmaker.DomainTypes
open System.Net.Mail

[<CliPrefix(CliPrefix.DoubleDash)>]
[<NoAppSettings>]
type private CliArgument =
    | CreateMentorshipMatches of csvDocumentPath: string
    | ConvertCsvToJson of csvDocumentPath: string
    | CreateMatches of applicantsJsonPath: string
    | MatchesToCsv of matchesJsonPath: string
    | UnmatchedDataDump of applicantsJsonPath: string
    | UnmatchedPermutationsDataDump of applicantsJsonPath: string
    | GenerateExampleEmails of matchesJsonPath: string
    | FromCsvToExampleEmails of csvDocumentPath: string
    | SendEmailsMatched of matchesJsonPath: string
    | ConcatenateEmailsUnmatched of applicantsJsonPath: string
with
    interface IArgParserTemplate with
        member cliArg.Usage =
            match cliArg with
            | CreateMentorshipMatches _ -> "Provide relative path the CSV document containing the current's round information with all the applicant's data."
            | ConvertCsvToJson _ -> "Provide relative path the CSV document containing the current's round information with all the applicant's data."
            | CreateMatches _ -> "Provide a relative path to the JSON document with applicant data."
            | MatchesToCsv _ -> "Provides a summary of the matches in CSV form."
            | UnmatchedDataDump _ -> "Provide a relative path to the JSON document with applicant data."
            | UnmatchedPermutationsDataDump _ -> "Provide a relative path to the JSON document with applicant data."
            | GenerateExampleEmails _ -> "Provide a relative path to the JSON document with matches data."
            | FromCsvToExampleEmails _ -> "Executes a lot of steps in sequence, itermediately writing ot files."
            | SendEmailsMatched _ -> "Send out e-mails to matches mentors and mentees."
            | ConcatenateEmailsUnmatched _ -> "Provides a concatenated string of e-mail address that can be used to send a batch e-mail to all unmatched mentees."

type InputValidationError =
    | InputMissing
    | RelativePathDoesNotExists of string
    | NoMatchPossible of string
with
    member x.ErrorMessage =
        match x with
        | InputMissing -> "The provided argument is missing/empty. Please provide a path for the file."
        | RelativePathDoesNotExists relativePath -> $"The relative path to the CSV document {relativePath} does not exists. Please check your input."
        | NoMatchPossible relativePath -> $"The provided file {relativePath} couldn't produce a single match between a mentor and a mentee. Please consult your data."


[<EntryPoint>]
let main argv =
    // Don't forget to provide the current CSV document for the mentorship.
    // Please leave the CSV document out of the repository. It's been excluded in the git ignore.
    // Don 't commit the file in the repository.
    let run (parsedArguments: ParseResults<CliArgument>) =
        match parsedArguments.GetAllResults() with
        | [] -> 
            Error InputMissing

        | toolMode :: tail ->
            match toolMode with
            | CreateMentorshipMatches(csvDocumentPath) ->
                if String.IsNullOrEmpty csvDocumentPath then
                    Error InputMissing
                elif File.Exists(csvDocumentPath) <> true then
                    Error (RelativePathDoesNotExists csvDocumentPath)
                else
                    let mentorshipPairings, plannerInputs =
                        csvDocumentPath
                        |> CsvExtractor.extractApplicantsInformation
                        |> Matchmaking.createPlannerInputs
                        |> Matchmaking.getMentorshipPairing

                    match mentorshipPairings with
                    | [] ->
                        Error (NoMatchPossible csvDocumentPath)
                
                    | _ ->
                        mentorshipPairings |> List.map EmailGenerationService.dumpTemplateEmailsInFile |> ignore
                        plannerInputs |> Matchmaking.dumpToFileUnmatchedApplicants
                        Ok ()

            | ConvertCsvToJson csvDocumentPath ->
                if String.IsNullOrEmpty csvDocumentPath then
                    Error InputMissing
                elif File.Exists(csvDocumentPath) <> true then
                    Error (RelativePathDoesNotExists csvDocumentPath)
                else
                    Matchmaking.convertToJson csvDocumentPath |> ignore

                    Ok ()

            | CreateMatches applicantsJsonPath ->
            
                if String.IsNullOrEmpty applicantsJsonPath then
                    Error InputMissing
                elif File.Exists(applicantsJsonPath) <> true then
                    Error (RelativePathDoesNotExists applicantsJsonPath)
                else
                    Matchmaking.createMatches applicantsJsonPath |> ignore

                    Ok ()

            | MatchesToCsv matchesJsonPath ->
                if String.IsNullOrEmpty matchesJsonPath then
                    Error InputMissing
                elif File.Exists(matchesJsonPath) <> true then
                    Error (RelativePathDoesNotExists matchesJsonPath)
                else
                    Matchmaking.matchesToCsv matchesJsonPath |> ignore

                    Ok ()


            | UnmatchedDataDump applicantsJsonPath ->
                
                if String.IsNullOrEmpty applicantsJsonPath then
                    Error InputMissing
                elif File.Exists(applicantsJsonPath) <> true then
                    Error (RelativePathDoesNotExists applicantsJsonPath)
                else
                    Matchmaking.unmatchedDataDump applicantsJsonPath |> ignore

                    Ok ()

            | UnmatchedPermutationsDataDump applicantsJsonPath ->
                           
                if String.IsNullOrEmpty applicantsJsonPath then
                    Error InputMissing
                elif File.Exists(applicantsJsonPath) <> true then
                    Error (RelativePathDoesNotExists applicantsJsonPath)
                else
                    Matchmaking.unmatchedPermutationsDataDump applicantsJsonPath |> ignore

                    Ok ()

            | GenerateExampleEmails matchesJsonPath ->
                
                if String.IsNullOrEmpty matchesJsonPath then
                    Error InputMissing
                elif File.Exists(matchesJsonPath) <> true then
                    Error (RelativePathDoesNotExists matchesJsonPath)
                else
                    Matchmaking.generateExampleEmails matchesJsonPath |> ignore

                    Ok ()

            | FromCsvToExampleEmails csvDocumentPath ->
                if String.IsNullOrEmpty csvDocumentPath then
                    Error InputMissing
                elif File.Exists(csvDocumentPath) <> true then
                    Error (RelativePathDoesNotExists csvDocumentPath)
                else
                    Matchmaking.convertToJson csvDocumentPath
                    |> Matchmaking.createMatches
                    |> Result.map (fun x ->
                        Matchmaking.unmatchedPermutationsDataDump x.unmatched |> ignore
                        Matchmaking.unmatchedDataDump x.unmatched |> ignore
                        Matchmaking.generateExampleEmails x.matches |> ignore)
                    |> ignore

                    Ok ()

            | SendEmailsMatched matchesJsonPath ->
                // Send mails to matched ppl
                Matchmaking.sendEmailsMatched matchesJsonPath |> ignore

                Ok ()
                
            | ConcatenateEmailsUnmatched applicantsJsonPath ->
                Matchmaking.concatenateEmailsUnmatched applicantsJsonPath |> ignore

                Ok ()

                


    let errorHandler = ProcessExiter(colorizer = function ErrorCode.HelpText -> None | _ -> Some System.ConsoleColor.Red)
    let cliArgumentParser = ArgumentParser.Create<CliArgument>(checkStructure = false, errorHandler = errorHandler, programName = "Mentor matchmaker") // Settings checkStructure to false blocks Argu from checking if the DU is properly formed -- avoids performance hit via Reflection.
    let resultFromRunningTool = (cliArgumentParser.ParseCommandLine argv) |> run

    match resultFromRunningTool with
    | Error error ->
        printfn $"{error.ErrorMessage}"
        -1
    | Ok _ ->
        0