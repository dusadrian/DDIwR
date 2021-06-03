\name{recodeMissing}

\alias{recodeMissing}

\title{Recode missing values}

\description{
A function to recode all missing values to either SPSS or Stata types,
uniformly (re)using the same codes across all variables.
}

\usage{
recodeMissing(dataset, to = c("SPSS", "Stata"), dictionary = NULL, ...)
}


\arguments{
  \item{dataset}{A data frame}
  \item{to}{Software to recode missing values for}
  \item{dictionary}{A named vector, with corresponding SPSS values and Stata codes.}
  \item{...}{Other internal arguments.}
}

\details{
When a dictionary is not provided, it is automatically constructed from the available
data and metadata, using negative numbers starting from -1 and up to 27 letters
starting with "a".

If the dataset contains mixed variables with SPSS and Stata style missing values,
unless otherwise specified in a dictionary it uses other codes than the existing ones.

For the SPSS type of missing values, the resulting variables are coerced to a mixed
labelled format.
}

\value{
A data frame with all missing values recoded.
}


\examples{
x <- data.frame(A = mixed_labelled(c(1:5, -9),
                              labels = c(Good = 1, Bad = 5, NR = -9),
                              na_values = -9),
                B = labelled(c(1:5, tagged_na('a')), labels = c(DK = tagged_na('a'))),
                C = mixed_labelled(c(1, -8, 3:5, -9),
                              labels = c(DK = -8, NR = -9),
                              na_values = c(-8, -9)))
#        A     B      C
# 1      1     1      1
# 2      2     2 NA(-8)
# 3      3     3      3
# 4      4     4      4
# 5      5     5      5
# 6 NA(-9) NA(a) NA(-9)


xrec <- recodeMissing(x, to = "Stata")

#       A     B     C
# 1     1     1     1
# 2     2     2 NA(b)
# 3     3     3     3
# 4     4     4     4
# 5     5     5     5
# 6 NA(c) NA(a) NA(c)


attr(xrec, "dictionary")
#  b  c 
# -8 -9


recodeMissing(x, to = "Stata", dictionary = c(a = -8, b = -9))
#       A     B     C
# 1     1     1     1
# 2     2     2 NA(a)
# 3     3     3     3
# 4     4     4     4
# 5     5     5     5
# 6 NA(b) NA(a) NA(b)


recodeMissing(x, to = "SPSS")
#        A      B      C
# 1      1      1      1
# 2      2      2 NA(-8)
# 3      3      3      3
# 4      4      4      4
# 5      5      5      5
# 6 NA(-9) NA(-1) NA(-9)


recodeMissing(x, to = "SPSS", dictionary = c(a = -8, b = -9))
#        A      B      C
# 1      1      1      1
# 2      2      2 NA(-8)
# 3      3      3      3
# 4      4      4      4
# 5      5      5      5
# 6 NA(-9) NA(-8) NA(-9)
}

\keyword{functions}
