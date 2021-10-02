module FileManager

open System.IO

(* Private *)

let rec private fileNameWithVersion (fileName : string) (extension: string) (version: int) =
    let versionedName = 
        if version = 0 then
            fileName + extension
        else
            fileName + "(" + version.ToString() + ")" + extension

    if File.Exists versionedName then
        fileNameWithVersion fileName extension (version + 1)
    else 
        versionedName
        
        
(* Public *)
        
/// Saves file as .txt, appends a version if file already exists
let saveVersionedText (fileName : string) (contents: string) =
    let outputFileName = fileNameWithVersion fileName ".txt" 0
    File.AppendAllText (outputFileName, contents)
    outputFileName

/// Saves file as .json, appends a version if file already exists
let saveVersionedJson<'a> (fileName : string) (contents : 'a) =
    let json = Thoth.Json.Net.Encode.Auto.toString(4, contents)
    let outputFileName = fileNameWithVersion fileName ".json" 0
    File.AppendAllText (outputFileName, json)
    outputFileName

/// Saves file as .csv, appends a version if file already exists
let saveVersionedCsv (fileName : string) (contents : string) =
    let outputFileName = fileNameWithVersion fileName ".csv" 0
    File.AppendAllText (outputFileName, contents)
    outputFileName

let readJson<'a> (fileName : string) =
    if File.Exists fileName then
        let json = File.ReadAllText fileName
        Thoth.Json.Net.Decode.Auto.fromString<'a> json
    else
        Error ("File " + fileName + " doesn't seem to exist. Check the provided path.")