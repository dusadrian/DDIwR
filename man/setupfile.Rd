\name{setupfile}

\alias{setupfile}

\title{Create setup files for SPSS, Stata, SAS and R}

\description{
This function creates a setup file, based on a list of variable and value labels.
}

\usage{
setupfile(obj = "", type = "all", csv = "", miss = "", uniqueid = "",
          SD = "", delimiter = ",", OS = "windows", outfile = "",
          forcenum = c(""), ...)
}


\arguments{
  \item{obj}{A list object containing the variables, along with their subcomponents
        labels and values, or a path to a directory where these objects are located,
        for batch processing.}
  \item{type}{The type of setup file, can be: "SPSS", "Stata", "SAS", "R",
        or "all" (default).}
  \item{csv}{The original dataset, on the basis of which the setup file commands
        are created, or a path to the directory where the .csv files are located,
        for batch processing.}
  \item{miss}{A vector of missing values, or missing labels, for automatic detection
        in case the input \bold{\code{obj}} does not contain this information.}
  \item{uniqueid}{Character, the name of the unique identifier variable}
  \item{SD}{The row delimiter for the Stata commands, can be for example "" or ";"}
  \item{delimiter}{The column delimiter to be used for reading the .csv file,
        default is ","}
  \item{OS}{The target operating system, for the eol - end of line separator.}
  \item{outfile}{Character, the name of the setup file being created.}
  \item{forcenum}{A vector of column names which should be forced as numeric.}
  \item{...}{Other arguments (not used in this function).}
}

\details{
If \bold{\code{type = "all"}}, it will produce once setup file for each supported type.
All created setup files will be saved in a directory called "Setup Files" which
(if not already found) will be created in the user's current working directory.

Missing values are expected to be supplied with the information object \bold{\code{obj}},
otherwise the argument\bold{ \code{miss}} expects either:\cr
- a vector of missing values (e.g. -1, -2, -3, if such values refer to
missings throughout the entire dataset), or\cr
- a vector of labels that should be interpreted as missings

If batch processing multiple files, the function will inspect all files in the
provided directory, and retain only those with the extension  \code{.R} or \code{.r}
or DDI versions with the extension \code{.xml} or \code{.XML} (it will
subsequently generate an error if the .R files do not contain an object list,
or if the \code{.xml} files do not contain a DDI structured metadata file).

If the metadata directory contains a subdirectory called \code{"data"} or
\code{"Data"}, it will match the name of the metadata file with the name of the
\code{.csv} file (their names have to be *exactly* the same, irrespective of
their extension).

The \bold{\code{csv}} argument can provide a data.frame object produced by reading the
\code{.csv} file, or a path to the directory where the \code{.csv} files are located.
If the user doesn't provide something for this argument, the function will check
the existence of a subdirectory called \code{data} in the directory where the metadata
files are located.

The \bold{\code{uniqueid}} argument is only relevant if \code{type = "R"}. It is necessary
to identify missing observations in different variables, based on the unique case
identifiers found in the variable provided via this argument. It will generate an
"attr"ibute called \code{"missing types"}, which is essentially a list whose components
are variable names, and each component is a list itself containing a vector of
values for each missing category (type) plus the identifiers of the cases where
missing values are found (and replaced with NA).
It will also generate an attribute called \code{"unique id"}, which points to the same
name of the variable containing the unique case identifiers.

If the information object \bold{\code{obj}} already contains the information about which
variable is the unique identifier, the argument \bold{\code{uniqueid}} is ignored.

The argument \code{SD} only makes sense when \code{type = "Stata"} or \code{type = "all"},
(when Stata setup files will also be generated).

In batch mode, the code starts with the argument \bold{\code{delimiter = ","}}, but if the
\code{.csv} file is delimited differently it will also try hard to find other delimiters
which will match the variable names in the metadata file. At the initial version 0.1-0,
the automatically detected delimiters include \code{";"} and \code{"\t"}.

The argument \code{OS} can be either:\cr
\code{"windows"} (default), or \code{"Windows"}, \code{"Win"}, \code{"win"},\cr
\code{"MacOS"}, \code{"Darwin"}, \code{"Apple"}, \code{"Mac"}, \code{"mac"},\cr
\code{"Linux"}, \code{"linux"}.

The end of line separator changes only when the target OS is different from the
running OS.

The argument \bold{\code{outfile}} expects the name of the final setup file being
saved on the disk. If nothing is provided, the name of the object provided for the
\code{obj} argument will be used as a filename.

The argument \bold{\code{forcenum}} forces imported character variables as numeric.
Sometimes, a clearly numerical variable might have string entries like "N.A." in the
csv file, thus when importing the entire variables it is detected as character
instead of numeric. All these string entries will be eliminated, leaving only
numbers which are suitable to force the variable as numeric.

There is also an undocumented, boolean argument called \bold{\code{saveFile}}, which
if set to \code{TRUE} it will save an R version if the metadata was read from a DDI
\code{.xml} file, in the same directory. This function uses \code{\link{getMetadata}},
where that argument is a formal one.
}

\value{
A setup file to complement the imported raw dataset.
}


\examples{
test <- list()

test$ID <- list(
    label = "Questionnaire ID",
    type = "num",
    measurement = "interval",
    uniqueid = TRUE
)

test$V1 <- list(
    label = "Label for the first variable",
    values = c(
        "No"             =  0, 
        "Yes"            =  1,
        "Not applicable" = -7,
        "Not answered"   = -9
        ),
    missing = c(-7, -9),
    type = "cat",
    measurement = "nominal"
)

test$V2 <- list(
    label = "Label for the second variable",
    values = c(
        "Very little"    =  1, 
        "Little"         =  2,
        "So, so"         =  3,
        "Much"           =  4,
        "Very much"      =  5,
        "Don't know"     = -8
        ),
    missing = c(-8),
    type = "cat",
    measurement = "ordinal"
)

test$V3 <- list(
    label = "Label for the third variable",
    values = c(
        "First answer"   = "A", 
        "Second answer"  = "B",
        "Don't know"     = -8
        ),
    missing = c(-8),
    type = "cat",
    measurement = "nominal"
)

test$V4 <- list(
    label = "Number of children",
    values = c(
        "Don't know"     = -8,
        "Not answered"   = -9
        ),
    missing = c(-8, -9),
    type = "numcat",
    measurement = "ratio"
)

\dontrun{
# IMPORTANT:
# make sure to set the working directory to a directory with read/write permissions
# setwd("/path/to/read/write/directory")

# path.to.csv <- file.path(system.file(package = "DDIwR"), "data", "test.csv.gz")


setupfile(test)


# if the csv data file is available
setupfile(test, csv="/path/to/csv/file.csv")


# generating a specific type of setup file
setupfile(test, type = "Stata", SD = ";")


# other types of possible utilizations, using paths to specific files
# an XML file containing a DDI metadata object

setupfile("/path/to/the/metadata/file.xml", csv="/path/to/csv/file.csv")


# if the metadata is saved to an .R file containing a list
setupfile("/path/to/the/metadata/file.R", csv="/path/to/csv/file.csv")


# or in batch mode, specifying entire directories
setupfile("/path/to/the/metadata/directory", csv="/path/to/csv/directory")
}
}

\keyword{functions}
