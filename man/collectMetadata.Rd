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
x <- data.frame(A = labelled_spss(c(1:5, -9),
                                  labels = c(Good = 1, Bad = 5, NR = -9),
                                  na_values = -9),
                B = labelled(c(1:5, tagged_na('a')), labels = c(DK = tagged_na('a'))),
                C = labelled_spss(c(1, -8, 3:5, -9),
                                  labels = c(DK = -8, NR = -9),
                                  na_values = c(-8, -9)))

collectMetadata(x)

}

\keyword{functions}
