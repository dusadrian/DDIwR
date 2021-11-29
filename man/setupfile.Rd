\name{setupfile}

\alias{setupfile}

\title{Create setup files for SPSS, Stata, SAS and R}

\description{
This function creates a setup file, based on a list of variable and value labels.
}

\usage{
setupfile(codeBook, file = "", type = "all", csv = "", OS = "", ...)
}


\arguments{
  \item{codeBook}{A list object containing the metadata, or a path to a directory where
        these objects are located, for batch processing}
  \item{file}{Character, the (path to the) setup file to be created}
  \item{type}{The type of setup file, can be: "SPSS", "Stata", "SAS", "R",
        or "all" (default)}
  \item{csv}{The original dataset, used to create the setup file commands, or a path
        to the directory where the .csv files are located, for batch processing}
  \item{OS}{The target operating system, for the eol - end of line character(s)}
  \item{...}{Other arguments, see Details below}
}

\details{
When the a path to a metadata directory is specified for the argument \bold{\code{codebook}},
then next argument \bold{\code{file}} is silently ignored and all created setup files are
saved in a directory called "Setup Files" that (if not already found) is created in the
working directory.

The argument \bold{\code{file}} expects the name of the final setup file being
saved on the disk. If not specified, the name of the object provided for the
\bold{\code{codebook}} argument will be used as a filename.

If \bold{\code{file}} is specified, the argument \bold{\code{type}} is automatically
determined from the file's extension, otherwise when \bold{\code{type = "all"}}, the
function produces one setup file for each supported type.

If batch processing multiple files, the function will inspect all files in the
provided directory, and retain only those with the extension  \code{.R} or \code{.r}
or DDI versions with the extension \code{.xml} or \code{.XML} (it will
subsequently generate an error if the .R files do not contain an object list,
or if the \code{.xml} files do not contain a DDI structured metadata file).

If the metadata directory contains a subdirectory called \code{"data"} or
\code{"Data"}, it will match the name of the metadata file with the name of the
\code{.csv} file (their names have to be *exactly* the same, regardless of
their extension).

The \bold{\code{csv}} argument can provide a data frame object produced by reading the
\code{.csv} file, or a path to the directory where the \code{.csv} files are located.
If the user doesn't provide something for this argument, the function will check
the existence of a subdirectory called \code{data} in the directory where the metadata
files are located.

In batch mode, the code starts with the argument \bold{\code{delim = ","}}, but if the
\code{.csv} file is delimited differently it will also try hard to find other delimiters
that will match the variable names in the metadata file. At the initial version 0.1-0,
the automatically detected delimiters include \code{";"} and \code{"\t"}.

The argument \code{OS} (case insensitive) can be either:\cr
\code{"Windows"} (default), or \code{"Win"},\cr
\code{"MacOS"}, \code{"Darwin"}, \code{"Apple"}, \code{"Mac"},\cr
\code{"Linux"}.\cr

The end of line character(s) changes only when the target OS is different from the
running OS.
}

\value{
A setup file to complement the imported raw dataset.
}


\examples{

codeBook <- list(dataDscr = list(
ID = list(
    label = "Questionnaire ID",
    type = "num",
    measurement = "interval"
),
V1 = list(
    label = "Label for the first variable",
    labels = c(
        "No"             =  0, 
        "Yes"            =  1,
        "Not applicable" = -97,
        "Not answered"   = -99),
    na_values = c(-99, -97),
    type = "cat",
    measurement = "nominal"
),
V2 = list(
    label = "Label for the second variable",
    labels = c(
        "Very little"    =  1, 
        "Little"         =  2,
        "So, so"         =  3,
        "Much"           =  4,
        "Very much"      =  5,
        "Don't know"     = -98),
    na_values = c(-98),
    type = "cat",
    measurement = "ordinal"
),
V3 = list(
    label = "Label for the third variable",
    labels = c(
        "First answer"   = "A", 
        "Second answer"  = "B",
        "Don't know"     = -98),
    na_values = c(-98),
    type = "cat",
    measurement = "nominal"
),
V4 = list(
    label = "Number of children",
    labels = c(
        "Don't know"     = -98,
        "Not answered"   = -99),
    na_values = c(-99, -98),
    type = "numcat",
    measurement = "ratio"
)))


\dontrun{
# IMPORTANT:
# make sure to set the working directory to a directory with read/write permissions
# setwd("/path/to/read/write/directory")

# path.to.csv <- file.path(system.file(package = "DDIwR"), "data", "test.csv.gz")


setupfile(codeBook)


# if the csv data file is available
setupfile(codeBook, csv="/path/to/csv/file.csv")


# generating a specific type of setup file
setupfile(codeBook, file = "codeBook.do") # type = "Stata" is unnecessary


# other types of possible utilizations, using paths to specific files
# an XML file containing a DDI metadata object

setupfile("/path/to/the/metadata/file.xml", csv="/path/to/csv/file.csv")


# or in batch mode, specifying entire directories
setupfile("/path/to/the/metadata/directory", csv="/path/to/csv/directory")
}
}

\keyword{functions}
