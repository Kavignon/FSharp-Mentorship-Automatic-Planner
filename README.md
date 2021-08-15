# The F# Software Foundation Mentorship Planner (FSSF Mentorship Planner)

## Context
The mentorship initiative for the F# community has been a great education effort. It helps the community
learn and share their expertise on functional programming. The issue with the program is the time it takes to put together the list of pairs of mentors and mentees. It would take 8 to 12 hours to sort through all the applications each round.

## Purpose of the tool
The FSSF Mentorship Planner is here to help the responsible of the mentorship program to bootstrap the program faster. It takes all the data that can be extracted from the Google Forms and generate the pairings automatically. Instead of taking hours, it takes a few moments after feeding the file with all the applications.

## How does the tool work?
It inspects the data and breaks it down into the following components:
-- The F# topics for the either the mentors or the mentees
-- The open availabilities during the week
-- The time zones of the applicants

Once that's done, it will separate the applicants in two categories: mentees and mentors. It looks for applicants that share common interests and an overlapping schedule in UTC. After that, it only outputs unique pairings and identifies applicants that could not be matched. Usually, there will be more applicants that we can possibly matched.

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


[Git]: https://www.git-scm.com/
[.NET 5.0 SDK]: https://dotnet.microsoft.com/download
