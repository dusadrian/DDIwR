\name{getMetadata}

\alias{getMetadata}

\title{Get metadata information from an XML file}

\description{
This function reads an XML file containing a DDI codebook version 2.5 and
returns the variable labels and value labels.
}

\usage{
getMetadata(xmlpath, OS = "windows", saveFile = FALSE, ...)
}

\arguments{
  \item{xmlpath}{A path to an XML file with a DDI codebook}
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
A list containing all variabiles in the DDI file, and their corresponding
variable labels, value labels (if applicable) and missing values if found.
}



\keyword{functions}
