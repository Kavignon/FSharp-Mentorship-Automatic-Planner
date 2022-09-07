open System
open System.IO
open MentorMatchmaker
open FsToolkit.ErrorHandling
open Argu

[<CliPrefix(CliPrefix.DoubleDash)>]
type EmailArgs =
    | [<AltCommandLine("-p")>] Password of password: string
with
    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | Password _ -> "SMTP password for mentorship@fsharp.org email account"

[<CliPrefix(CliPrefix.DoubleDash)>]
type OutputArgs =
    | [<AltCommandLine("-r")>] Results
    | [<AltCommandLine("-e")>] Email
    | [<AltCommandLine("-f")>] File of string
    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | Results _ -> "Output the results of the paired applicants with topics and unpaired applicants"
            | Email _ -> "Output the emails that will be sent with the *Email* option"
            | File _ -> "File path to write the output to instead of the standard output"

[<CliPrefix(CliPrefix.None)>]
type CliArgument =
    | [<MainCommand;ExactlyOnce;First>] Input of csvDocumentPath: string
    | [<Last>]Output of ParseResults<OutputArgs>
    | [<Last>]Email of ParseResults<EmailArgs>
with
    interface IArgParserTemplate with
        member cliArg.Usage =
            match cliArg with
            | Input _ -> "Provide relative path the CSV document containing the current's round information with all the applicant's data."
            | Email _ -> "Send emails out to applicants. Password required with the -p|--password flag. Output can be checked with the Output command"
            | Output _ -> "Write the output of the mentorship pairings to stdout"

[<EntryPoint>]
let main argv =
    // Don't forget to provide the current CSV document for the mentorship.
    // Please leave the CSV document out of the repository. It's been excluded in the git ignore.
    // Don 't commit the file in the repository.

    let mainArgParser =
        ArgumentParser.Create<CliArgument>(
            // Setting checkStructure to false blocks Argu from checking if the DU is properly formed -- avoids performance hit via Reflection.
            checkStructure = false,
            errorHandler = ProcessExiter(
                colorizer =
                    function
                    | ErrorCode.HelpText -> None
                    | _ -> Some ConsoleColor.Red
            ),
            programName = "Mentor matchmaker"
        )

    let mainParsedCommands = mainArgParser.ParseCommandLine(argv)

    result {
        let csvPath = mainParsedCommands.GetResult Input
        if not (System.IO.File.Exists(csvPath)) then
            return! Error $"File missing: {csvPath}"

        use stream = File.OpenRead csvPath

        let! applicantPool =
            ApplicantInput.readApplicantPool stream
            |> Result.mapError (fun (ApplicantInput.InvalidInput (invalidInput, message)) ->
                $"Input Parsing Error: {invalidInput}; {message}")

        let pairs, unpaired = MatchMaking.matchApplicants applicantPool

        if mainParsedCommands.Contains Email then
            let parser = mainParsedCommands.GetResult Email
            let password = parser.GetResult Password
            use smtpClient = EmailGeneration.createSmtpClient password
            for pair in pairs do
                EmailGeneration.sendEmailToPairedApplicants smtpClient pair

        if mainParsedCommands.Contains Output then
            let parser = mainParsedCommands.GetResult Output
            use writer: TextWriter =
                if parser.Contains File then
                    let path = parser.GetResult File
                    let stream = File.OpenWrite(path)
                    printfn $"Writing to {Path.GetFullPath path}"
                    new StreamWriter(stream)
                else
                    Console.Out

            if parser.Contains Results then
                let fullName applicant = applicant.PersonalInformation.FullName

                fprintf writer  $"Paired: {pairs.Length}; "
                fprintf writer  $"Unpaired Mentors: {unpaired.Mentors.Length}; "
                fprintfn writer $"Unpaired Mentees: {unpaired.Mentees.Length}"

                writer.WriteLine()

                fprintfn writer "Pairs:"
                for { Mentor = mentor; Mentee = mentee; MutualTopics = topics; MutualAvailabilities = availabilities} in pairs do
                    fprintf writer $"  {fullName mentor} | {fullName mentee}: "
                    fprintfn writer "[ %s ]" (String.Join("; ", topics))
                    let offsetUtcLabel = function
                        | Zero -> "UTC"
                        | Positive n -> $"UTC + {n}"
                        | Negative n -> $"UTC - {n * -1}"

                    let toLocalPrintedRanges offset (weektimes:Set<WeekTime>) =
                        weektimes
                        |> Set.map (fun a -> a.AddHours(offset))
                        |> toWeekTimeRanges
                        |> Seq.map (fun range -> $"{range.Start.Weekday}: {range.Start.Time:hht}-{range.End.Time:hht}")
                        |> String.concat "; "

                    fprintfn writer "    Mentor Times: (%s) { %s }"
                        (offsetUtcLabel mentor.PersonalInformation.LocalOffset)
                        (toLocalPrintedRanges mentor.PersonalInformation.LocalOffset availabilities)

                    fprintfn writer "    Mentee Times: (%s) { %s }"
                        (offsetUtcLabel mentee.PersonalInformation.LocalOffset)
                        (toLocalPrintedRanges mentee.PersonalInformation.LocalOffset availabilities)

                writer.WriteLine()

                fprintfn writer "Unpaired Mentors:"
                for mentor in unpaired.Mentors do
                    fprintfn writer $"  {fullName mentor}"
                writer.WriteLine()

                fprintfn writer "Unpaired Mentees:"
                for mentee in unpaired.Mentees do
                    fprintfn writer $"  {fullName mentee}"
                writer.WriteLine()

            if parser.Contains OutputArgs.Email then
                fprintfn writer $"Emails:\n{EmailGeneration.generateEmailExamples pairs}"
                writer.WriteLine()

    }
    |> function
        | Ok () -> 0
        | Error message -> eprintfn $"Error: {message}"; 1
