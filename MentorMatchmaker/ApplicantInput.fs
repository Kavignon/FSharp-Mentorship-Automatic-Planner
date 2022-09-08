[<RequireQualifiedAccess>]
module MentorMatchmaker.ApplicantInput

open System
open type StringSplitOptions
open FSharp.Data

open Domain
open MentorMatchmaker
open Microsoft.FSharp.Core.CompilerServices
open FsToolkit.ErrorHandling

type MentorshipInformation = CsvProvider<"mentorship_schema_file.csv">
type InvalidInput = InvalidInput of invalidInput:string * message:string

[<AutoOpen>]
module Functions =

    let generateWeekList (text:string) startHour endHour = [
        let weekdays =
            text.Split([|','; ';'|], TrimEntries ||| RemoveEmptyEntries)
            |> Array.choose Option.tryParse<DayOfWeek>

        for weekday in weekdays do
            for hour in startHour .. endHour - 1 do
                yield { Weekday = weekday; Time = TimeOnly(hour, 0, 0) }
    ]

    let convertTextToTopic topicText =
        let deepDiveInFSharpKeywords =
            [ "Deep";
              "dive";
              "investment";
              "better" ]

        let mobileDevelopmentKeywords =
            [ "Uno";
              "Fabulous";
              "Xamarin";
              "Mobile";
              "Mobile development" ]

        let distributedSystemKeywords =
            [ "Microservices";
              "Distributed systems";
              "event sourcing" ]

        let webDevelopmentKeywords =
            [ "Web";
              "Elmish";
              "Fable";
              "SAFE";
              "Giraffe";
              "React";
              "Feliz";
              "MVC";
              "Web development / SAFE stack" ]

        let openSourceKeywords =
            [ "Contribute to an open source project";
              "open source" ]

        let matchOnTopicText (topicText: string) =
            let doesCategoryMatchKeyword (categoryName: string) (keywordList: string list) =
                keywordList
                |> List.exists
                    (fun keyword -> categoryName.Contains(keyword, StringComparison.InvariantCultureIgnoreCase))

            if String.Equals("Introduction to F#", topicText, StringComparison.InvariantCultureIgnoreCase)
               || topicText.Contains("beginner", StringComparison.InvariantCultureIgnoreCase)  then
                IntroductionToFSharp
            elif
                String.Equals
                    (
                        "Contribute to an open source project",
                        topicText,
                        StringComparison.InvariantCultureIgnoreCase
                    )
            then
                ContributeToOpenSource
            elif String.Equals("Machine learning", topicText, StringComparison.InvariantCultureIgnoreCase) then
                MachineLearning
            elif String.Equals("Contribute to the compiler", topicText, StringComparison.InvariantCultureIgnoreCase) then
                ContributeToCompiler
            elif String.Equals("Designing with types", topicText, StringComparison.InvariantCultureIgnoreCase) then
                DesigningWithTypes
            elif String.Equals("Meta programming", topicText, StringComparison.InvariantCultureIgnoreCase) then
                MetaProgramming
            elif String.Equals("Domain modeling", topicText, StringComparison.InvariantCultureIgnoreCase) then
                DomainModelling
            elif topicText.Equals("up for anything", StringComparison.InvariantCultureIgnoreCase) || topicText.Contains("All of the above", StringComparison.InvariantCultureIgnoreCase) then
                UpForAnything
            elif doesCategoryMatchKeyword topicText openSourceKeywords then
                ContributeToOpenSource
            elif doesCategoryMatchKeyword topicText deepDiveInFSharpKeywords then
                DeepDiveInFSharp
            elif doesCategoryMatchKeyword topicText mobileDevelopmentKeywords then
                MobileDevelopment
            elif doesCategoryMatchKeyword topicText distributedSystemKeywords then
                DistributedSystems
            elif doesCategoryMatchKeyword topicText webDevelopmentKeywords then
                WebDevelopment
            else
                UpForAnything

        if String.IsNullOrEmpty topicText then
            Error (InvalidInput(topicText, "No topics were found"))
        else
            topicText.Split([|','; ';'|], TrimEntries ||| RemoveEmptyEntries)
            |> Seq.map matchOnTopicText
            |> Set
            |> Ok

    let getUtcOffset = function
        | "UTC" -> Ok 0
        | RegexGroupValue "UTC \+ (\d+)" value -> Ok (int value)
        | RegexGroupValue "UTC \- (\d+)" value -> Ok -(int value)
        | text -> Error (InvalidInput ("Invalid Parsing UTC Offset", text))

    type InputApplicant =
        | Mentor of Applicant
        | Mentee of Applicant

    let rowToInputApplicant (row: MentorshipInformation.Row) : Result<InputApplicant,_> =
        let result = result {
            let! offset = getUtcOffset row.``What is your time zone?``

            let applicantInfo =
                { FullName = row.``What is your full name (First and Last Name)``
                  SlackName = row.``What is your fsharp.org slack name?``
                  EmailAddress = row.``Email address``
                  LocalOffset = offset }

            let availabilities =
                Set [
                    yield! generateWeekList row.``What time are you available? [09:00 - 12:00 local time]`` 9 12
                    yield! generateWeekList row.``What time are you available? [12:00 - 15:00 local time]`` 12 15
                    yield! generateWeekList row.``What time are you available? [15:00 - 18:00 local time]`` 15 18
                    yield! generateWeekList row.``What time are you available? [18:00 - 21:00 local time]`` 18 21
                    yield! generateWeekList row.``What time are you available? [21:00 - 00:00 local time]`` 21 24
                ]
                |> Set.map (fun weekTime -> weekTime.AddHours(-offset))
                |> toWeekTimeRanges

            return!
                match row.``I want to be a`` with
                | IgnoreCase "mentor" -> result {
                    let! topics = convertTextToTopic row.``What topics do you feel comfortable mentoring?``
                    return Mentor({
                        PersonalInformation = applicantInfo
                        Topics = topics
                        Availabilities = availabilities
                    })}
                | IgnoreCase "mentee" -> result {
                    let! topics = convertTextToTopic row.``What topic do you want to learn?``
                    return Mentee({
                        PersonalInformation = applicantInfo
                        Topics = topics
                        Availabilities = availabilities
                    })}
                | other -> Error(InvalidInput (other, $"'{nameof(row.``I want to be a``)}' column was not correct"))
        }

        result
        |> Result.mapError (fun (InvalidInput(data, message)) ->
            InvalidInput(data, message + " for applicant: " + row.``What is your full name (First and Last Name)``)
        )

let readApplicantPool (stream: System.IO.Stream) : Result<ApplicantPool,InvalidInput> = result {
    use data = MentorshipInformation.Load stream

    let mutable mentors = ListCollector()
    let mutable mentees = ListCollector()

    for row in data.Rows do
        match! rowToInputApplicant row with
        | Mentor applicant -> mentors.Add(applicant)
        | Mentee applicant -> mentees.Add(applicant)

    return { Mentors = mentors.Close(); Mentees = mentees.Close() }
}
