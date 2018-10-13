\name{exportDDI}

\alias{exportDDI}

\title{Export to a DDI metadata file}

\description{
This function creates a DDI version 2.5, XML file structure.
}

\usage{
exportDDI(codebook, file = "", embed = TRUE, OS = "", indent = 4)
}


\arguments{
  \item{codebook}{A list object containing the metadata, or a path to a directory where
        these objects are located, for batch processing}
  \item{file}{either a character string naming a file or a connection open for
              writing. "" indicates output to the console.}
  \item{embed}{Embed the CSV datafile in the XML file, if present.}
  \item{OS}{The target operating system, for the eol - end of line character(s)}
  \item{indent}{Indent width, in number of spaces}
}

\details{
The information object is essentially a list having two main list components:

- \bold{\code{fileDscr}}, if the data is provided in a subcomponent named
\bold{\code{datafile}}

- \bold{\code{dataDscr}}, having as many components as the number of variables
in the (meta)data. For each variable, there should a mandatory subcomponent called
\bold{\code{label}} (that contains the variable's label) and, if the variable is of
a categorical type, another subcomponent called \bold{\code{values}}.

Additional informations about the variables can be specified as further subcomponents,
combining DDI specific data but also other information that might not be covered
by DDI:

- \bold{\code{measurement}} is the equivalent of the specific DDI attribute 
\bold{\code{nature}} of the \bold{\code{var}} element, and it accepts these values:
\code{"nominal"}, \code{"ordinal"}, \code{"interval"}, \code{"ratio"},
\code{"percent"}, and \code{"other"}.

- \bold{\code{type}} is useful for multiple reasons. A first one, if the variable is
numerical, is to differentiate between \code{discrete} and \code{contin} values of
the attribute \bold{\code{intrvl}} from the same DDI element \bold{\code{var}}.
Another reason is to help identifying pure string variables (containing text), when
the subcomponent \bold{\code{type}} is equal to \code{"char"}. It is also used for
the subelement \bold{\code{varFormat}} of the element \bold{\code{var}}.
Finally, another reason is to differentiate between pure categorical (\code{"cat"})
and pure numerical (\code{"num"}) variables, as well as mixed ones, among which
\code{"numcat"} referring to a numerical variable with very few values (such as the
number of children), for which it is possible to also produce a table of frequencies
along the numerical summaries. There are also categorical variables that can be
interpreted as numeric (\code{"catnum"}), such as a Likert type response scale with
7 values, where numerical summaries are also routinely performed along with the usual
table of frequencies.

- \bold{\code{missing}} is an important subcomponent, indicating which of the
values in the variable are going to be treated as missing values, and it is going
to be exported as the attribute \code{missing} of the DDI subelement
\bold{\code{catgry}}.


There are many more possible attributes and DDI elements to be added in the
information object, future versions of this function will likely expand.

For the moment, only DDI codebook version 2.5 is exported, but DDI Lifecycle is
also possible.


The argument \bold{\code{OS}} can be either:\cr
\code{"windows"} (default), or \code{"Windows"}, \code{"Win"}, \code{"win"},\cr
\code{"MacOS"}, \code{"Darwin"}, \code{"Apple"}, \code{"Mac"}, \code{"mac"},\cr
\code{"Linux"}, \code{"linux"}.

The end of line separator changes only when the target OS is different from the
running OS.

The argument \bold{\code{indent}} controls how many spaces will be used in the XML
file, to indent the different subelements.
}

\value{
An XML file containing a DDI version 2.5 metadata.
}

\seealso{
\url{http://www.ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/field_level_documentation.html}
}


\examples{
codeBook <- list(dataDscr = list(
ID = list(
    label = "Questionnaire ID",
    type = "num",
    measurement = "interval",
    uniqueid = TRUE
),
V1 = list(
    label = "Label for the first variable",
    values = c(
        "No"             =  0, 
        "Yes"            =  1,
        "Not applicable" = -7,
        "Not answered"   = -9),
    missing = c(-9, -7),
    type = "cat",
    measurement = "nominal"
),
V2 = list(
    label = "Label for the second variable",
    values = c(
        "Very little"    =  1, 
        "Little"         =  2,
        "So, so"         =  3,
        "Much"           =  4,
        "Very much"      =  5,
        "Don't know"     = -8),
    missing = c(-8),
    type = "cat",
    measurement = "ordinal"
),
V3 = list(
    label = "Label for the third variable",
    values = c(
        "First answer"   = "A", 
        "Second answer"  = "B",
        "Don't know"     = -8),
    missing = c(-8),
    type = "cat",
    measurement = "nominal"
),
V4 = list(
    label = "Number of children",
    values = c(
        "Don't know"     = -8,
        "Not answered"   = -9),
    missing = c(-9, -8),
    type = "numcat",
    measurement = "ratio"
),
V5 = list(
    label = "Political party reference",
    type = "char",
    txt = "When the respondent indicated his political party reference, his/her open response
was recoded on a scale of 1-99 with parties with a left-wing orientation coded on the low end
of the scale and parties with a right-wing orientation coded on the high end of the scale.
Categories 90-99 were reserved miscellaneous responses."
)))

exportDDI(codeBook, file = "codebook.xml")

}

\keyword{functions}
