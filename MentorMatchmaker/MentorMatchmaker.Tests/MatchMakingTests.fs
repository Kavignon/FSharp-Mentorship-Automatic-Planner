module MentorMatchmaker.Tests.MatchMakingTests

open MentorMatchmaker
open System
open type DayOfWeek
open NUnit.Framework
open FsUnit

type MentorName = string
type MenteeName = string
type ApplicantName = string
type Hour = int
type Availabilities = (DayOfWeek * Hour) list
type TestMatchModel = MentorName * MenteeName * Availabilities * Topic list
type TestUnpairedPool = {
    Mentors: ApplicantName list
    Mentees: ApplicantName list
}

type TestMatchesModel = {
    Pairs: TestMatchModel list
    NotPaired: TestUnpairedPool
}

let equalMatches (model:TestMatchesModel) = equal model

type TestMatchesModelNames = {
    PairedNames: (MentorName * MenteeName) list
    NotPairedNames: ApplicantName list
}

let equalNames (names:TestMatchesModelNames) = equal names

let toTestModel (pairs: MentorshipPair list, unpaired : ApplicantPool) =
    { Pairs = [
        for pair in pairs ->
            ( pair.Mentor.PersonalInformation.FullName,
              pair.Mentee.PersonalInformation.FullName,
              [ for availability in pair.MutualAvailabilities -> availability.Weekday, availability.Time.Hour ],
              Set.toList pair.MutualTopics )
      ]
      NotPaired =
        { Mentors = [ for applicant in unpaired.Mentors -> applicant.PersonalInformation.FullName ]
          Mentees = [ for applicant in unpaired.Mentees -> applicant.PersonalInformation.FullName ] } }


let toTestNames (pairs: MentorshipPair list, unpaired : ApplicantPool) =
    { PairedNames = [
        for pair in pairs ->
            ( pair.Mentor.PersonalInformation.FullName,
              pair.Mentee.PersonalInformation.FullName )
      ]
      NotPairedNames = [
          for applicant in unpaired.Mentors do
              yield applicant.PersonalInformation.FullName

          for applicant in unpaired.Mentees do
              yield applicant.PersonalInformation.FullName
      ]
    }

let toPairs pairs = { Pairs = pairs; NotPaired = { Mentors = []; Mentees = [] } }
let toNotPaired notPaired = { Pairs = []; NotPaired = notPaired }

module Generate =
    let personalInformation name =
        { FullName = name
          SlackName = name
          EmailAddress = name
          LocalOffset = 0 }

    let availability weekday startHour endHour =
        { Start = { Weekday = weekday; Time = TimeOnly(startHour, 0, 0) }
          End = { Weekday = weekday; Time = TimeOnly(endHour, 0, 0) } }

    let applicant name dayOfWeek topic =
        { PersonalInformation = personalInformation name
          Availabilities = [ availability dayOfWeek 12 12 ]
          Topics = Set [ topic ] }

module SUT =
    let matchApplicants = MatchMaking.matchApplicants >> toTestModel
    let applicantMatches = MatchMaking.matchApplicants >> toTestModel >> fun { Pairs = pairs } -> pairs
    let applicantsUnmatched = MatchMaking.matchApplicants >> toTestModel >> fun { NotPaired = notPaired } -> notPaired
    let matchApplicantNames = MatchMaking.matchApplicants >> toTestNames

// Mentor{x} is intended to match with Mentee{y} where x = y
let mentor1 = Generate.applicant "Mentor1" Sunday IntroductionToFSharp
let mentee1 = Generate.applicant "Mentee1" Sunday IntroductionToFSharp

let mentor2 = Generate.applicant "Mentor2" Monday DeepDiveInFSharp
let mentee2 = Generate.applicant "Mentee2" Monday DeepDiveInFSharp

let mentor3 = Generate.applicant "Mentor3" Tuesday WebDevelopment
let mentee3 = Generate.applicant "Mentee3" Tuesday WebDevelopment

[<Test>]
let ``Only pair mentors and mentees on matching topics`` () =
    SUT.matchApplicants
        { Mentors =
            [ mentor1
              mentor2
              { mentor3 with Topics = Set [ IntroductionToFSharp ] } ]
          Mentees =
            [ mentee1
              mentee2
              { mentee3 with Topics = Set [ DeepDiveInFSharp ] } ] }
    |> should equal {
        Pairs =
            [ "Mentor1", "Mentee1", [ (Sunday, 12) ], [ IntroductionToFSharp ]
              "Mentor2", "Mentee2", [ (Monday, 12) ], [ DeepDiveInFSharp ] ]
        NotPaired =
            { Mentors = ["Mentor3"]
              Mentees = ["Mentee3"] }
    }

[<Test>]
let ``Only pair mentors and mentees with matching Availabilities`` () =
    SUT.matchApplicants
      { Mentors =
          [ mentor1
            mentor2
            { mentor3 with Availabilities = [ Generate.availability Tuesday 12 12 ] } ]
        Mentees =
          [ mentee1
            mentee2
            { mentee3 with Availabilities = [ Generate.availability Wednesday 12 12 ] } ] }
    |> should equal {
        Pairs =
            [ "Mentor1", "Mentee1", [ (Sunday, 12) ], [ IntroductionToFSharp ]
              "Mentor2", "Mentee2", [ (Monday, 12) ], [ DeepDiveInFSharp ] ]
        NotPaired =
            { Mentors = ["Mentor3"]
              Mentees = ["Mentee3"] }
    }

[<Test>]
let ``Pair mentors with mentees with availabilities in the same range`` () =
    SUT.matchApplicants
      { Mentors = [ { mentor1 with Availabilities = [ Generate.availability Sunday 12 16 ] } ]
        Mentees = [ { mentee1 with Availabilities = [ Generate.availability Sunday 15 18 ] } ] }
    |> should equal {
        Pairs = [ "Mentor1", "Mentee1", [ (Sunday, 15); (Sunday, 16) ], [ IntroductionToFSharp ] ]
        NotPaired = { Mentors = []; Mentees = [] }
    }

[<Test>]
let ``Show preference for rarer topics (ContributeToCompiler) over Common topics (IntroductionToFSharp)`` () =
    let applicant name topics =
        { PersonalInformation = Generate.personalInformation name
          Availabilities = [ Generate.availability Sunday 12 12 ]
          Topics = Set.ofList topics }
    SUT.matchApplicantNames
      { Mentors =
          [ applicant "Mentor1" [ IntroductionToFSharp; ContributeToCompiler ] ]
        Mentees =
          [ applicant "MenteeCommon1" [ IntroductionToFSharp ]
            applicant "MenteeRare2" [ ContributeToCompiler ]
            applicant "MenteeCommon3" [ IntroductionToFSharp ] ] }
    |> should equalNames {
        PairedNames = [ ("Mentor1", "MenteeRare2") ]
        NotPairedNames = [ "MenteeCommon1"; "MenteeCommon3" ]
    }
