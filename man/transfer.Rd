\name{transfer}

\alias{transfer}

\title{Transfer / convert between software packages}

\description{
Something similar to StatTransfer
}

\usage{
transfer(from, to, embed = FALSE, executable = "", ...)
}

\arguments{
  \item{from}{A path to a file, or a tibble object}
  \item{to}{Character, the name of a software package or a path to a specific file}
  \item{embed}{Boolean, embed the data when generating a DDI XML file}
  \item{executable}{Path to the executable file to run a batch script}
  \item{...}{Additional parameters passed to exporting functions, see the Details section}
}

\details{
When the argument \bold{\code{to}} specifies a certain statistical package (\code{"R"},
\code{"Stata"}, \code{"SPSS"} or \code{"SAS"}), the name of the destination file will be the same
as the name of the input file from the argument \bold{\code{to}}, with an automatically added
software specific extension.

Alternatively, the argument \bold{\code{to}} can be specified as a path to a specific file, in
which case the software package is determined from its file extension. The following extentions
are currently recognized: \code{.xml}, \code{.R}, \code{.dta}, \code{.sav} and \code{.sas7bdat}.

The argument \bold{\code{executable}} is used only for the Stata and SAS softwares, which unlike
the rest of the software, have specific missing values using letters from \code{a} to \code{z}.
The usual transfer to these two software uses the default settings from package \bold{\code{haven}}
that do not convert the missing values by default. Specifying the path to the executable file is
also a Boolean signal to attempt converting these missing values via an automatic batch script that
recodes all unique missing values to the same letters, the lowest numerical value being assigned
to the letter \code{a}.

Additional parameters can be specified via the three dots argument \bold{\code{...}}, that are
passed to the respective functions from package \bold{\code{haven}}. For instance the function
\bold{\code{write_dta()}} has an additional argument called \bold{\code{version}} (from 8 to a
maximum and default value of 14) when writing a Stata file.
}



\keyword{functions}
