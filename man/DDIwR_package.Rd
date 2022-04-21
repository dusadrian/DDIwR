\name{About the DDIwR package}

\alias{DDIwR}

\docType{package}

\title{
Useful functions for various DDI (Data Documentation Initiative) related outputs.
}

\description{
This package provides various functions to read DDI based metadata documentation,
and write dedicated setup files for R, SPSS, Stata and SAS to read an associated
.csv file containing the raw data, apply labels for variables and values and
also deal with the treatment of missing values.

It can also generate a DDI metadata file out of an R information object, which can be
used to export directly to the standard statistical packages files (such as SPSS, Stata
and SAS, or even Excel), using the versatile package \bold{\pkg{haven}}. For R, the default
object to store data and metadata is a \bold{\code{data.frame}}, and labelled data are
automatically coerced to class \code{declared}.

The research leading to the initial functions in this package has received funding from
the European Union's Seventh Framework Programme (FP7/2007-2013) under grant agreement no.
262608 (DwB - Data without Boundaries)
}

\details{
    \tabular{ll}{
    Package: \tab DDIwR\cr
    Type:    \tab Package\cr
    Version: \tab 0.9\cr
    Date:    \tab 2021-09-04\cr
    License: \tab GPL (>= 2)\cr
    }
}

\author{

\bold{Authors}:\cr
Adrian Dusa\cr
Department of Sociology\cr
University of Bucharest\cr
\email{dusa.adrian@unibuc.ro}

\bold{Maintainer}:\cr
Adrian Dusa
}

\keyword{package}
