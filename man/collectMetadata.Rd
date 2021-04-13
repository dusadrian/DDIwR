\name{collectMetadata}

\alias{collectMetadata}

\title{Collect metadata}

\description{
Collect metadata from a data frame (preferably a tibble) containing labelled variables.
}

\usage{
collectMetadata(dataset)
}


\arguments{
  \item{dataset}{A data frame / tibble}
}

\value{
An R list containing the metadata collected from each variable.
}


\examples{
require(labelled)
x <- data.frame(A = labelled(c(1:5, -9), labels = c(Good = 1, Bad = 5, NR = -9)))
na_values(x) <- -9

collectMetadata(x)

}

\keyword{functions}
