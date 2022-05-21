# DDIwR

*DDI with R*

## Description

This is a package to collect data and metadata from a variety of sources, and
convert those into some of the most popular statistical packages: Stata, SAS,
SPSS, and naturally R.

The biggest strength of this package is its ability to convert to and from a
DDI ([Data Documentation Initiative](https://ddialliance.org)) Codebook.

## Handling missing values

The problem of the missing values <del>is</del> was a recurrent one in R, with
various solutions having been implemented in various places. The base package
offers a single (empty, undeclared) type of missing value, the `NA`.

In many sciences, however, there are multiple reasons for missing values, and
those are not covered by the `NA`.  
A dedicated package was written to solve this, called
[declared](https://cran.r-project.org/web/packages/declared/index.html), which
is used by **DDIwR**.

## Graphical user interface

Package **DDIwR** benefits from a cross-platform GUI called
[StatConverter](http://roda.github.io/StatConverter/). It is similar to a popular
commercial software, but built on top of R.

![](images/StatConverter.png?raw=true)

Windows users can already download the setup installer (including R, embedded in
the application), or download a .zip file containing all application files to
start the application without installing it. More versions for different other
operating systems will soon be added.
