\name{getMetadata}

\alias{getMetadata}

\title{Extract metadata information}

\description{
This function reads an XML file containing a DDI codebook version 2.5, or an
SPSS file and returns a list containing the variable labels, value labels,
plus some other useful information.
}

\usage{
getMetadata(x, OS = "windows", saveFile = FALSE, ...)
}

\arguments{
  \item{x}{A path to a file, or a tibble object}
  \item{OS}{The target operating system, for the eol - end of line separator}
  \item{saveFile}{Boolean, save an .R file in the same folder}
  \item{...}{Additional arguments for this function (internal uses only)}
}

\details{
The argument \code{OS} can be either:\cr
\code{"windows"} (default), or \code{"Windows"}, \code{"Win"}, \code{"win"},\cr
\code{"MacOS"}, \code{"Darwin"}, \code{"Apple"}, \code{"Mac"}, \code{"mac"},\cr
\code{"Linux"}, \code{"linux"}.\cr

The end of line separator changes only when the target OS is different from the
running OS.

For the moment, only DDI version 2.5 (Codebook) is supported, but DDI version 3.2
(Lifecycle) is planned to be implemented.
}


\value{
A list containing all variables, their corresponding variable labels and value labels,
and (if applicable) missing values if found.
}



\keyword{functions}
