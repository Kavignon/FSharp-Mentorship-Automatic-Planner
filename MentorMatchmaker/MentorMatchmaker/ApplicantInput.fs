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
            |> Array.choose (Option.tryParse<DayOfWeek>)
            |> Array.toList

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
            let category =
                if topicText[0] = ' ' then
                    topicText.Substring(1)
                else
                    topicText
            if String.Equals("Introduction to F#", category, StringComparison.InvariantCultureIgnoreCase)
               || category.Contains("beginner", StringComparison.InvariantCultureIgnoreCase)  then
                IntroductionToFSharp
            elif
                String.Equals
                    (
                        "Contribute to an open source project",
                        category,
                        StringComparison.InvariantCultureIgnoreCase
                    )
            then
                ContributeToOpenSource
            elif String.Equals("Machine learning", category, StringComparison.InvariantCultureIgnoreCase) then
                MachineLearning
            elif String.Equals("Contribute to the compiler", category, StringComparison.InvariantCultureIgnoreCase) then
                ContributeToCompiler
            elif String.Equals("Designing with types", category, StringComparison.InvariantCultureIgnoreCase) then
                DesigningWithTypes
            elif String.Equals("Meta programming", category, StringComparison.InvariantCultureIgnoreCase) then
                MetaProgramming
            elif String.Equals("Domain modeling", category, StringComparison.InvariantCultureIgnoreCase) then
                DomainModelling
            elif category.Equals("up for anything", StringComparison.InvariantCultureIgnoreCase) || category.Contains("All of the above", StringComparison.InvariantCultureIgnoreCase) then
                UpForAnything
            elif doesCategoryMatchKeyword category openSourceKeywords then
                ContributeToOpenSource
            elif doesCategoryMatchKeyword category deepDiveInFSharpKeywords then
                DeepDiveInFSharp
            elif doesCategoryMatchKeyword category mobileDevelopmentKeywords then
                MobileDevelopment
            elif doesCategoryMatchKeyword category distributedSystemKeywords then
                DistributedSystems
            elif doesCategoryMatchKeyword category webDevelopmentKeywords then
                WebDevelopment
            else
                UpForAnything

        if String.IsNullOrEmpty topicText then
            Error (InvalidInput(topicText, "No topics were found"))
        elif topicText.Contains(',') <> true then
            Ok (Set [ matchOnTopicText topicText ])
        else
            topicText.Split(',', StringSplitOptions.TrimEntries ||| StringSplitOptions.RemoveEmptyEntries)
            |> Array.map matchOnTopicText
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

            let availabilites =
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
                        Availabilities = availabilites
                    })}
                | IgnoreCase "mentee" -> result {
                    let! topics = convertTextToTopic row.``What topic do you want to learn?``
                    return Mentee({
                        PersonalInformation = applicantInfo
                        Topics = topics
                        Availabilities = availabilites
                    })}
                | other -> Error(InvalidInput (other, $"'{nameof(row.``I want to be a``)}' column was not correct"))
        }

        result
        |> Result.mapError (fun (InvalidInput(data, message)) ->
            InvalidInput(data, message + " for applicant: " + row.``What is your full name (First and Last Name)``))

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
