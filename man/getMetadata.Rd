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
  \item{x}{A path to a file, or a tibble object}
  \item{save}{Boolean, save an .R file in the same directory}
  \item{OS}{The target operating system, for the eol - end of line separator,
            if saving the file}
  \item{...}{Additional arguments for this function (internal uses only)}
}

\details{
This function reads an XML file containing a DDI codebook version 2.5, or an
SPSS file and returns a list containing the variable labels, value labels,
plus some other useful information.

It is also possible to extract a limited information from a Stata file, but
especially the missing values are not yet imported by package \bold{\pkg{haven}}.


It additionally attempts to automatically detect a type for each variable:
\tabular{rl}{
  \bold{\code{cat}}: \tab categorical\cr
  \bold{\code{num}}: \tab numerical\cr
  \bold{\code{numcat}}: \tab numerical variable with very few values (ex. number of children)\cr
  \tab for which a table of frequencies is possible in addition to frequencies
}

By default, this function extracts the metadata into an R list object, but when the argument
\code{save} is activated, the argument \code{OS} (case insensitive) can be either:\cr
\code{"Windows"} (default), or \code{"Win"},\cr
\code{"MacOS"}, \code{"Darwin"}, \code{"Apple"}, \code{"Mac"},\cr
\code{"Linux"}.\cr

The end of line separator changes only when the target OS is different from the
running OS.

For the moment, only DDI version 2.5 (Codebook) is supported, but DDI version 3.2
(Lifecycle) is planned to be implemented.
}


\value{
A list containing all variables, their corresponding variable labels and value labels,
and (if applicable) missing values if imported and found.
}

\author{
Adrian Dusa
}



\keyword{functions}
