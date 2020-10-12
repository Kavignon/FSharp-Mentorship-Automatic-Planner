[<RequireQualifiedAccess>]
module MentorshipMatchmaker.App

open System

open Argu

open AppHandlers
open AppValidation

let private validateAndHandleDocumentPath path =
    match path with
    | FilePathNullOrWhiteSpace ->
        Error InputMissing
    | FileNotFound ->
        Error (RelativePathDoesNotExists path)
    | FileFound ->
        match ClientHandlers.handle (ValidPath path) with
        | Ok matches -> Ok matches
        | Error matchmakingError -> Error (MatchmakingFailed matchmakingError)

// Don't forget to provide the current CSV document for the mentorship.
// Please leave the CSV document out of the repository. It's been excluded in the git ignore.
// Don 't commit the file in the repository.
let private run (parsedArguments: ParseResults<CliArgument>) =
    match parsedArguments.GetAllResults() with
    | [] ->
        Error InputMissing

    | toolMode :: _ ->
        match toolMode with
        | CreateMentorshipMatches csvDocumentPath -> validateAndHandleDocumentPath csvDocumentPath

let private errorColorizer = function
    | ErrorCode.HelpText -> None
    | _ -> Some ConsoleColor.Red

let parseArgsAndRun argv =
    let errorHandler = ProcessExiter(colorizer = errorColorizer)
    // Settings checkStructure to false blocks Argu from checking if the DU is properly formed
    // -- avoids performance hit via Reflection.
    let cliArgumentParser = ArgumentParser.Create<CliArgument>(checkStructure = false, errorHandler = errorHandler, programName = "Mentor matchmaker")
    let resultFromRunningTool = (cliArgumentParser.ParseCommandLine argv) |> run
    match resultFromRunningTool with
    | Error error ->
        printfn "%A" (error.ErrorMessage)
        -1
    | Ok mentorshipMatches ->
        0
