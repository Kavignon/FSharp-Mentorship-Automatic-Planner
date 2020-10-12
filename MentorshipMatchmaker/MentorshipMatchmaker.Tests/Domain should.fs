module ``Domain should``

open FsCheck.Xunit

open Swensen.Unquote

open MentorshipMatchmaker.Domain


[<Property>]
let ``return no match if there is no mentor`` mentees =
    let actual = Matchmaker.matchApplicants mentees []
    test<@ actual = None @>


[<Property>]
let ``return no match if there is no mentees`` mentors =
    let actual = Matchmaker.matchApplicants [] mentors
    test<@ actual = None @>
