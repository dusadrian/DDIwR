\name{recodeValues}

\alias{recodeValues}

\title{Recode missing values}

\description{
A function to recode all missing values to either SPSS or Stata types,
uniformly (re)using the same codes across all variables.
}

\usage{
recodeValues(dataset, to = c("SPSS", "Stata"), dictionary = NULL, chartonum = TRUE, ...)
}


\arguments{
  \item{dataset}{A data frame}
  \item{to}{Software to recode missing values for}
  \item{dictionary}{A named vector, with corresponding SPSS values and Stata codes.}
  \item{chartonum}{Logical, replace character values with numbers.}
  \item{...}{Other internal arguments.}
}

\details{
When a dictionary is not provided, it is automatically constructed from the
available data and metadata, using negative numbers starting from -91 and up to
27 letters starting with "a".

If the dataset contains mixed variables with SPSS and Stata style missing
values, unless otherwise specified in a dictionary it uses other codes than the
existing ones.

For the SPSS type of missing values, the resulting variables are coerced to a
declared labelled format.

Unlike SPSS, Stata does not allow labels for character values. Both cannot be
transported from SPSS to Stata, it is either one or another. If labels are
more important to preserve than original values (especially the information
about the missing values), the argument \code{chartonum} replaces all character
values with suitable, non-overlapping numbers and adjusts the labels accordingly.

If no labels are found in the metadata, the original values are preserved.
}

\value{
A data frame with all missing values recoded consistently.
}


\examples{
x <- data.frame(
    A = declared(
        c(1:5, -92),
        labels = c(Good = 1, Bad = 5, NR = -92),
        na_values = -92
    ),
    B = labelled(
        c(1:5, tagged_na('a')),
        labels = c(DK = tagged_na('a'))
    ),
    C = declared(
        c(1, -91, 3:5, -92),
        labels = c(DK = -91, NR = -92),
        na_values = c(-91, -92)
    )
)

#         A     B       C
# 1       1     1       1
# 2       2     2 NA(-91)
# 3       3     3       3
# 4       4     4       4
# 5       5     5       5
# 6 NA(-92) NA(a) NA(-92)


xrec <- recodeValues(x, to = "Stata")

#       A     B     C
# 1     1     1     1
# 2     2     2 NA(b)
# 3     3     3     3
# 4     4     4     4
# 5     5     5     5
# 6 NA(c) NA(a) NA(c)


attr(xrec, "dictionary")
#   b   c 
# -91 -92


recodeValues(x, to = "Stata", dictionary = c(a = -91, b = -92))
#       A     B     C
# 1     1     1     1
# 2     2     2 NA(a)
# 3     3     3     3
# 4     4     4     4
# 5     5     5     5
# 6 NA(b) NA(a) NA(b)


recodeValues(x, to = "SPSS")
#         A       B       C
# 1       1       1       1
# 2       2       2 NA(-91)
# 3       3       3       3
# 4       4       4       4
# 5       5       5       5
# 6 NA(-92) NA(-93) NA(-92)


recodeValues(x, to = "SPSS", dictionary = c(a = -91))
#         A       B       C
# 1       1       1       1
# 2       2       2 NA(-91)
# 3       3       3       3
# 4       4       4       4
# 5       5       5       5
# 6 NA(-92) NA(-91) NA(-92)
}

\keyword{functions}
