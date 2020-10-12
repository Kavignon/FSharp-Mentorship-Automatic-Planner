module ``Domain should``

open FsCheck
open FsCheck.Xunit

open Swensen.Unquote

open MentorshipMatchmaker.Infra
open MentorshipMatchmaker.Domain


[<Property>]
let ``return no matches if there is nobody`` () =
    let actual = Matchmaker.matchApplicants [] []
    test<@ actual = None @>

[<Property>]
let ``return no matches if there is no mentor`` mentees =
    let actual = Matchmaker.matchApplicants mentees []
    test<@ actual = None @>

[<Property>]
let ``return no matches if there is no mentee`` mentors =
    let actual = Matchmaker.matchApplicants [] mentors
    test<@ actual = None @>


type OnlyMatchingApplicants = OnlyMatchingApplicants of (Mentor list * Mentee list)
type OnlyNonMatchingApplicants = MatchingApplicants of (Mentor list * Mentee list)

type DomainArbitrary =

    static member OnlyMatchingApplicants() =
       Arb.from<Mentor list>
    static member OnlyNonMatchingApplicants() =
        Arb.from<Mentor list>


[<Property(Arbitrary = [| typeof<DomainArbitrary> |])>]
let ``return N matches if there is N mentors available slots matching N mentees wishes``
    mentees mentors  =
    let actual = Matchmaker.matchApplicants mentees mentors
    test<@ actual = failwith "wip" @>

[<Property(Arbitrary = [| typeof<DomainArbitrary> |])>]
let ``return no matches if there is 0 mentors available slots matching N mentees wishes``
    mentees mentors  =
    let actual = Matchmaker.matchApplicants mentees mentors
    test<@ actual = failwith "wip" @>
