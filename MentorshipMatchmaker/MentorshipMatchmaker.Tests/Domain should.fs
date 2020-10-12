module ``Domain should``

open FsCheck.Xunit
open MentorshipMatchmaker.Domain
open Xunit

[<Property>]
let ``match nobody if there is no mentors`` mentees =
    Matchmaker.matchMenteesWithMentors

    ()
