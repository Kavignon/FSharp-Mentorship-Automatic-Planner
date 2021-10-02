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
dotnet run --creatementorshipmatches mentorship_data_file.csv
```

The program creates two files containing:
* the emails to send to the mentor and the mentee for each matching pair
  `templateEmailToSendDump.txt`
* the list of applicants without a match
  `applicationDataDump.txt`


## Schema of the CSV File
To create an input file conforming to the schema used by the program,
you should use the [`mentorship_schema_file.csv`] as a starting point.

### Columns
*Timestamp*<br>
The date at wich the application was received (e.g. `9/7/2020 21:47`)

*Email Address*<br>
The email address of the applicant

*What is your full name (First and Last Name)*<br>
The full name of the applicant

*What is your fsharp.org slack name?*<br>
The name of the applicant on the FSSF Slack server

*I want to be a*<br>
The role to which the applicant applied to (`mentor` or `mentee`)

*What topics do you feel comfortable mentoring?*<br>
A comma-separated list of topics of interest (for mentors only)

*What topic do you want to learn?*<br>
A comma-separated list of topics of interest (for mentees only)

*Why do you think you are good candidate for the mentorship program?*<br>
Unused

*What is your time zone?*<br>
The time zone of the applicant (e.g. `UTC`, `UTC+2`, `UTC-4`)

*What time are you available? [09:00 - 12:00 local time]*<br>
A comma-separated list of days on which the applicant is available
between 09:00 and 12:00 (e.g `Saturday, Sunday`)

*What time are you available? [12:00 - 15:00 local time]*<br>
A comma-separated list of days on which the applicant is available
between 12:00 and 15:00 (e.g `Saturday, Sunday`)

*What time are you available? [15:00 - 18:00 local time]*<br>
A comma-separated list of days on which the applicant is available
between 15:00 and 18:00 (e.g `Saturday, Sunday`)

*What time are you available? [18:00 - 21:00 local time]*<br>
A comma-separated list of days on which the applicant is available
between 18:00 and 21:00 (e.g `Saturday, Sunday`)

*What time are you available? [21:00 - 00:00 local time]*<br>
A comma-separated list of days on which the applicant is available
between 21:00 and 00:00 (e.g `Saturday, Sunday`)

### Topics of Interest
*Categories*<br>
Introduction to F#, Contribute to an open source project, Contribute to
the compiler, Machine learning, Up for anything

*Deep Dive in F# Keywords*<br>
Deep, dive, investment, better

*Mobile Development Keywords*<br>
Uno, Fabulous, Xamarin, Mobile

*Distributed System Keywords*<br>
Microservices, Distributed Systems, event sourcing

*Web Development Keywords*<br>
Web, Elmish, Fable, SAFE, Giraffe, React, Feliz, MVC


[F# Software Foundation]: https://fsharp.org/
[mentorship program]: https://fsharp.org/mentorship/
[Git]: https://www.git-scm.com/
[.NET 5.0 SDK]: https://dotnet.microsoft.com/download
[`mentorship_schema_file.csv`]: MentorMatchmaker/MentorMatchmaker/mentorship_schema_file.csv
