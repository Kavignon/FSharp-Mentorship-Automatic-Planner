# FSSF Mentorship Planner


## Description
The *FSSF Mentorship Planner* is a tool used to pair mentors with
mentees depending on their schedules and topics of interest. It is used
by the [F# Software Foundation] as part of its [mentorship program].

### About the F# Software Foundation
The FSSF is a community-operated, not-for-profit organization dedicated
to the growth, support, and education of a diverse community around the
F# programming language.

### About the Mentorship Program
In order to further education, the FSSF operates a mentorship program
that connects experienced F# developers to those wishing to improve
their programming skills and learn the best practices in F#.


## Usage
In order to use the *Mentorship Planner*, ensure that you have [Git] and
the [.NET 5.0 SDK] installed. Then run the following commands in your
shell:

```
git clone https://github.com/Kavignon/FSharp-Mentorship-Automatic-Planner.git
cd FSharp-Mentorship-Automatic-Planner/MentorMatchmaker/MentorMatchmaker
dotnet run --creatementorshipmatches file.csv
```

The program creates two files:
* `templateEmailToSendDump.txt` which contains, for each matching pair,
  the emails to send to the mentor and the mentee;
* `applicationDataDump.txt` which contains the list of applicants
  without a match.


[F# Software Foundation]: https://fsharp.org/
[mentorship program]: https://fsharp.org/mentorship/
[Git]: https://www.git-scm.com/
[.NET 5.0 SDK]: https://dotnet.microsoft.com/download
