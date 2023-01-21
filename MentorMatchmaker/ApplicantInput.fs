[<RequireQualifiedAccess>]
module MentorMatchmaker.ApplicantInput

open System
open type StringSplitOptions
open FSharp.Data

open Domain
open MentorMatchmaker
open Microsoft.FSharp.Core.CompilerServices
open FsToolkit.ErrorHandling

type MentorshipInformation = JsonProvider<"mentorship_schema_file.json">
type InvalidInput = InvalidInput of invalidInput:string * message:string

[<AutoOpen>]
module Functions =

    let generateWeekList (weekdays:string array) startHour endHour = [
        for weekday in weekdays do
            match DayOfWeek.TryParse(weekday, ignoreCase=true) with
            | true, weekday -> 
                for hour in startHour .. endHour - 1 do
                    yield { Weekday = weekday; Time = TimeOnly(hour, 0, 0) }
            | false, _ -> ()
    ]

    let convertTextToTopics topicsText =
        let mutable okCollector = ListCollector()
        let mutable errorCollector = ListCollector()
        
        for topicText in topicsText do
            match topicText with
            | "intro" -> okCollector.Add IntroductionToFSharp
            | "deep_dive" -> okCollector.Add DeepDiveInFSharp
            | "web" -> okCollector.Add WebDevelopment
            | "ml" -> okCollector.Add MachineLearning
            | "game_dev" -> okCollector.Add GameDevelopment
            | "devops" -> okCollector.Add Devops
            | "open_source" -> okCollector.Add ContributeToOpenSource
            | "compiler" -> okCollector.Add ContributeToCompiler
            | "mobile" -> okCollector.Add MobileDevelopment
            | "meta" -> okCollector.Add MetaProgramming
            | "domain_modelling" -> okCollector.Add DomainModelling
            | "up_for_anything" -> okCollector.Add UpForAnything
            | other -> errorCollector.Add $"Missing Topic: {other}"
            
        let oks = okCollector.Close()
        let errors = errorCollector.Close()
            
        if errors.IsEmpty then
            Ok oks
        else
            Error (InvalidInput ("topics", String.concat "\n" errors))

    type InputApplicantType =
        | Mentor of Applicant
        | Mentee of Applicant

    let rowToInputApplicant (info: MentorshipInformation.Root) : Result<InputApplicantType,_> =
        let result = result {
            let! utcOffset =
                if -12 <= info.UtcOffset && info.UtcOffset <= 12 then
                    Ok info.UtcOffset
                else
                    Error (InvalidInput("utc_offset", "UTC Offset invalid Range"))
            let applicantInfo =
                { FullName = info.Name
                  SlackName = ""
                  EmailAddress = info.Email
                  LocalOffset = utcOffset }

            let availabilities =
                Set [
                    yield! generateWeekList info.Schedule12Am 0 3
                    yield! generateWeekList info.Schedule3Am 3 6
                    yield! generateWeekList info.Schedule6Am 6 9
                    yield! generateWeekList info.Schedule9Am 9 12
                    yield! generateWeekList info.Schedule12Pm 12 15
                    yield! generateWeekList info.Schedule3Pm 15 18
                    yield! generateWeekList info.Schedule6Pm 18 21
                    yield! generateWeekList info.Schedule9Pm 21 24
                ]
                |> Set.map (fun weekTime -> weekTime.AddHours(-utcOffset))
                |> toWeekTimeRanges

            return!
                match info.ApplicantType with
                | IgnoreCase "mentor" -> result {
                    let! topics =
                        convertTextToTopics info.Topics
                    return Mentor({
                        PersonalInformation = applicantInfo
                        Topics = Set topics
                        Availabilities = availabilities
                    })}
                | IgnoreCase "mentee" -> result {
                    let! topics =
                        convertTextToTopics info.Topics
                    return Mentee({
                        PersonalInformation = applicantInfo
                        Topics = Set topics
                        Availabilities = availabilities
                    })}
                | other -> Error(InvalidInput (other, $"'{nameof(info.ApplicantType)}' column was not correct"))
        }

        result
        |> Result.mapError (fun (InvalidInput(data, message)) ->
            InvalidInput(data, message + " for applicant: " + info.Name)
        )

let readApplicantPool (stream: System.IO.Stream) : Result<ApplicantPool,InvalidInput> = result {
    let data = MentorshipInformation.Load stream

    let mutable mentors = ListCollector()
    let mutable mentees = ListCollector()

    for root in data do
        match! rowToInputApplicant root with
        | Mentor applicant -> mentors.Add(applicant)
        | Mentee applicant -> mentees.Add(applicant)

    return { Mentors = mentors.Close(); Mentees = mentees.Close() }
}
