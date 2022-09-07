module MentorMatchmaker.MatchMaking

open MentorMatchmaker
open Flips
open Flips.Types
open Flips.SliceMap

(*

MatchMaking is done using linear programming by creating:
    * A list of decisions (mentor/mentee pair) an algorithm can turn on or off
    * An objective  (create as many pairs as possible with the rarest topics)
        an algorithm watches to get as high as possible
    * Constraints on pairings (do not allow mentors or mentees to be matched up more than once)
        an algorithm is not allowed to exceed

*)

let private toTopicWeight topic : float =
    let Anything = 1
    let Common = 3
    let Popular = 5
    let Rare = 10

    match topic with
    | IntroductionToFSharp -> Common
    | DeepDiveInFSharp -> Popular
    | ContributeToOpenSource -> Popular
    | WebDevelopment -> Popular
    | ContributeToCompiler -> Rare
    | MachineLearning -> Rare
    | DistributedSystems -> Rare
    | MobileDevelopment -> Rare
    | DomainModelling -> Popular
    | UpForAnything -> Anything
    | DesigningWithTypes -> Rare
    | MetaProgramming -> Rare

type private ApplicantData = {
    Id: int
    Applicant: Applicant
    Topics: Set<Topic>
    WeekTimes: Set<WeekTime>
}

type private MentorId = int
type private MenteeId = int

let private toApplicantData id applicant =
    { Id = id
      Applicant = applicant
      WeekTimes = toWeekTimes applicant.Availabilities
      Topics = applicant.Topics }

type UnpairedApplicants = ApplicantPool

let matchApplicants (applicantPool:ApplicantPool) : MentorshipPair list * UnpairedApplicants =
    let mentors, mentees =
        let mutable i = 0
        ( [ for mentor in applicantPool.Mentors do toApplicantData i mentor; i <- i + 1 ],
          [ for mentee in applicantPool.Mentees do toApplicantData i mentee; i <- i + 1 ] )

    let matchablePairs = [
        for mentor in mentors do
            for mentee in mentees do
                if (Set.overlaps mentor.Topics mentee.Topics || mentor.Topics.Contains UpForAnything || mentee.Topics.Contains UpForAnything)
                   && Set.overlaps mentor.WeekTimes mentee.WeekTimes then
                    yield mentor, mentee
    ]

    let mentorMenteeKey mentor mentee = $"{mentor.Id},{mentee.Id}"

    let decisions = SMap2<MentorId, MenteeId, Decision> [
        for mentor, mentee in matchablePairs do
            yield (mentor.Id, mentee.Id), Decision.createBoolean (mentorMenteeKey mentor mentee)
    ]

    let combineTopics mentorTopics menteeTopics =
        match Set.contains UpForAnything mentorTopics, Set.contains UpForAnything menteeTopics with
        | true, true
        | false, false -> Set.intersect mentorTopics menteeTopics
        | true, false -> menteeTopics
        | false, true -> mentorTopics

    let topicWeight = SMap2<MentorId, MenteeId, float> [
        for mentor, mentee in matchablePairs do
            let topicScore =
                combineTopics mentor.Topics mentee.Topics
                |> Set.map toTopicWeight
                |> Set.maxElement
            yield (mentor.Id, mentee.Id), topicScore
    ]

    let model =
        Model.create (Objective.create "MaximizeTopicWeight" Maximize (sum(decisions .* topicWeight)))
        |> Model.addConstraints [
            for mentor in mentors do
                yield Constraint.create $"{mentor.Id}" ((sum decisions[mentor.Id, All]) <== 1.0)

            for mentee in mentees do
                yield Constraint.create $"{mentee.Id}" ((sum decisions[All, mentee.Id]) <== 1.0)
        ]

    match Solver.solve Settings.basic model with
    | Optimal result ->
        let mentorMenteeMap = Map [
            for mentor in mentors do
                for mentee in mentees do
                    mentorMenteeKey mentor mentee, (mentor, mentee)
        ]

        let matches = [
            for KeyValue({ Name = DecisionName name }, value) in result.DecisionResults do
                if value > 0 then
                    Map.find name mentorMenteeMap
        ]

        let matchedMentorIds = Set [ for mentor, _ in matches do mentor.Id ]
        let matchedMenteeIds = Set [ for _, mentee in matches do mentee.Id ]

        let pairs : MentorshipPair list = [
            for mentor, mentee in matches do
            yield {
                Mentor = mentor.Applicant
                Mentee = mentee.Applicant
                MutualAvailabilities = Set.intersect mentor.WeekTimes mentee.WeekTimes
                MutualTopics = combineTopics mentor.Topics mentee.Topics
            }
        ]

        let unmatchedApplicants : UnpairedApplicants = {
            Mentors = [
                for mentor in mentors do
                    if not (Set.contains mentor.Id matchedMentorIds) then
                        yield mentor.Applicant
            ]
            Mentees = [
                for mentee in mentees do
                    if not (Set.contains mentee.Id matchedMenteeIds) then
                        yield mentee.Applicant
            ]
        }

        pairs, unmatchedApplicants
    | other -> failwith (sprintf "%A" other)
