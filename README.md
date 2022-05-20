# DDIwR

*DDI with R*

## Description

This is a package to collect data and metadata from a variety of sources, and
convert those into some of the most popular statistical packages: Stata, SAS,
SPSS, and naturally R.

The biggest strength of this package is its ability to convert to and from a
DDI Codebook. DDI standts for [Data Documentation Initiative](https://ddialliance.org).

## Handling missing values

The problem of the missing values is recurrent in R, with various solutions
having been implemented so far. The base package offers a single (empty,
undeclared) type of missing value, the `NA`.

In many sciences, however, there are multiple reasons why a value is missing,
and those are not covered by the `NA`. For this reason, a dedicated package was
written called [declared](https://cran.r-project.org/web/packages/declared/index.html),
which is used by *DDIwR*.

## Graphical user interface

This package benefits from a cross-platform GUI called [StatConverter](http://roda.github.io/StatConverter/).
It is very similar to another popular, but commercial software, but built on top
of R.

Windows users can already download the setup installer, or download a .zip file
containing all application files to start the application without installing it.
More versions for different other operating systems will soon be added.
