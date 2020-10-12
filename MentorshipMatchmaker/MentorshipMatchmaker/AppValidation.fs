module MentorshipMatchmaker.AppValidation

open System
open System.IO

open Argu

open MentorshipMatchmaker.AppHandlers


let (|FilePathNullOrWhiteSpace|FileNotFound|FileFound|) filePath =
    if String.IsNullOrWhiteSpace(filePath) then FilePathNullOrWhiteSpace
    elif File.Exists(filePath) <> true then FileNotFound
    else FileFound

[<CliPrefix(CliPrefix.DoubleDash)>]
[<NoAppSettings>]
type CliArgument =
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
    | MatchmakingFailed of MatchmakingError
with
    member x.ErrorMessage =
        match x with
        | InputMissing ->
            "The provided argument is either missing or empty.\n
             Please provide a path for the file."
        | RelativePathDoesNotExists relativePath ->
            sprintf
                "The relative path to the CSV document %A does not exists.\n
                 Please check your input."
                relativePath
        | MatchmakingFailed matchmakingError ->
            sprintf
                "The provided file %A couldn't produce a single match between a mentor and a mentee.\n
                 Please consult your data."
                matchmakingError
