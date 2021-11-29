\name{getMetadata}

\alias{getMetadata}

\title{Extract metadata information}

\description{
Extract a list containing the variable labels, value labels and any available
information about missing values.
}

\usage{
getMetadata(x, save = FALSE, OS = "Windows", ...)
}

\arguments{
  \item{x}{A path to a file, or a data frame object}
  \item{save}{Boolean, save an .R file in the same directory}
  \item{OS}{The target operating system, for the eol - end of line separator,
            if saving the file}
  \item{...}{Additional arguments for this function (internal uses only)}
}

\details{
This function reads an XML file containing a DDI codebook version 2.5, or an
SPSS or Stata file and returns a list containing the variable labels, value
labels, plus some other useful information.


It additionally attempts to automatically detect a type for each variable:
\tabular{rl}{
  \bold{\code{cat}}: \tab categorical using numeric values\cr
  \bold{\code{catchar}}: \tab categorical using character values\cr
  \bold{\code{num}}: \tab numerical\cr
  \bold{\code{numcat}}: \tab numerical variable with very few values (ex. number of children)\cr
  \tab for which a table of frequencies is possible in addition to frequencies
}

By default, this function extracts the metadata into an R list object, but when
the argument \code{save} is activated, the argument \code{OS} (case insensitive)
can be either:\cr
\code{"Windows"} (default), or \code{"Win"},\cr
\code{"MacOS"}, \code{"Darwin"}, \code{"Apple"}, \code{"Mac"},\cr
\code{"Linux"}.\cr

The end of line separator changes only when the target OS is different from the
running OS.

For the moment, only DDI version 2.5 (Codebook) is supported, but DDI Lifecycle
is planned to be implemented.
}


\value{
A list containing all variables, their corresponding variable labels and value
labels, and (if applicable) missing values if imported and found.
}

\author{
Adrian Dusa
}


\examples{
x <- data.frame(
    A = declared(
        c(1:5, -92),
        labels = c(Good = 1, Bad = 5, NR = -92),
        na_values = -92
    ),
    C = declared(
        c(1, -91, 3:5, -92),
        labels = c(DK = -91, NR = -92),
        na_values = c(-91, -92)
    )
)

getMetadata(x)$dataDscr

}




\keyword{functions}
