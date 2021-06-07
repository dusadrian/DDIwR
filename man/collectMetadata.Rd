\name{collectMetadata}

\alias{collectMetadata}

\title{Collect metadata}

\description{
Collect metadata from a data frame containing labelled variables.
}

\usage{
collectMetadata(dataset)
}


\arguments{
\item{dataset}{A data frame}
}

\value{
An R list containing the metadata collected from each variable.
}


\examples{
x <- data.frame(A = declared(c(1:5, -9),
                        labels = c(Good = 1, Bad = 5, NR = -9),
                        na_values = -9),
                B = declared(c(1, -8, 3:5, -9),
                        labels = c(DK = -8, NR = -9),
                        na_values = c(-8, -9)))

collectMetadata(x)

}

\keyword{functions}
