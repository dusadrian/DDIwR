\name{convert}

\alias{convert}

\title{Convert a dataset from one statistical software to another}

\description{
This function converts (or transfers) between R, Stata, SPSS, SAS, Excel and DDI
XML files. Unlike the regular import / export functions from packages
\bold{\pkg{haven}} or \bold{\pkg{rio}}, this function uses the DDI standard as
an exchange platform and facilitates a consistent conversion of the missing values.
}

\usage{
convert(from, to = NULL, declared = TRUE, embed = TRUE, ...)
}

\arguments{
  \item{from}{A path to a file, or a data frame object}
  \item{to}{Character, the name of a software package or a path to a specific file}
  \item{declared}{Logical, return the resulting data frame as a declared object}
  \item{embed}{Boolean, embed the data when generating a DDI XML file}
  \item{...}{Additional parameters passed to exporting functions, see the Details section}
}

\details{
When the argument \bold{\code{to}} specifies a certain statistical package
(\code{"R"}, \code{"Stata"}, \code{"SPSS"}, \code{"SAS"}) or \code{"Excel"}, the
name of the destination file will be idential to the one in the argument
\bold{\code{from}}, with an automatically added software specific extension.

Alternatively, the argument \bold{\code{to}} can be specified as a path to a
specific file, in which case the software package is determined from its file
extension. The following extentions are currently recognized: \code{.xml} for
DDI, \code{.rds} for R, \code{.dta} for Stata, \code{.sav} for SPSS,
\code{.sas7bdat} for SAS, and \code{.xlsx} for Excel.

Additional parameters can be specified via the three dots argument
\bold{\code{...}}, that are passed to the respective functions from packages
\bold{\pkg{haven}} and \bold{\pkg{readxl}}. For instance the function
\bold{\code{\link[haven]{write_dta}()}} has an additional argument called
\bold{\code{version}} when writing a Stata file.

The most important argument to consider is called \bold{\code{user_na}}, part of
the function \bold{\code{\link[haven]{read_spss}()}}. Although it is defaulted
to \code{FALSE} in package \bold{\pkg{haven}}, in package \bold{\pkg{DDIwR}} it
is used as having the value of \code{TRUE}. Users who really want to deactivate
it should explicitly specify \code{use_na = FALSE} in function 
\bold{\code{{convert}()}}.


If the argument \bold{\code{to}} is left to \code{NULL}, the data is (invisibly)
returned to the R enviroment. Conversion to R, either in the working space or as
a data file, will result (by default) in a data frame containing declared
labelled variables, as defined in package \pkg{declared}.

The current version reads and creates DDI Codebook version 2.5, with future
versions to extend the functionality for DDI Lifecycle versions 3.x and link to
the future package \bold{DDI4R} for the UML model based version 4. It extends
the standard DDI Codebook by offering the possibility to embed a CSV version of
the raw data into the XML file containing the Codebook, into a \code{notes}
child of the \code{fileDscr} component. This type of Codebook is unique to this
package and automatically detected when converting to another statistical
software.

Converting the missing values to SAS is not tested, but it relies on the same
package \bold{\pkg{haven}} using the ReadStat C library. Should it not work, it
is also possible to use a setup file produced by function
\bold{\code{\link{setupfile}()}} and run the commands manually.

When the argument \bold{\code{embed}} is deactivated, a CSV file will be
produced in the same directory, using the same file name as the XML file.
}


\author{
Adrian Dusa
}

\references{
DDI - Data Documentation Initiative, see
\href{https://ddialliance.org/}{https://ddialliance.org/}
}


\seealso{
  \code{\link{setupfile}},
  \code{\link{getMetadata}},
  \code{\link[declared]{declared}},
  \code{\link[haven]{labelled}}
} 


\examples{
\dontrun{
# Assuming an SPSS file called test.sav is located in the working directory
# the following command will extract the metadata in a DDI Codebook and
# produce a test.xml file in the same directory
convert("test.sav", to = "DDI")

# It is possible to include the data in the XML file, using:
convert("test.sav", to = "DDI", embed = TRUE)

# To produce a Stata file:
convert("test.sav", to = "Stata")

# To produce an R file:
convert("test.sav", to = "R")

# To produce an Excel file:
convert("test.sav", to = "Excel")
}
}

\keyword{functions}
