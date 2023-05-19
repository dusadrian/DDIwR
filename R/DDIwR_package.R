#' @docType package
#'
#' @name DDIwR_package
#'
#' @title Useful functions for various DDI (Data Documentation Initiative)
#' related outputs.
#'
#' @description This package provides various functions to read DDI based
#' metadata documentation and write dedicated setup files for R, SPSS, Stata and
#' SAS to read an associated .csv file containing the raw data, apply labels for
#' variables and values and also deal with the treatment of missing values.
#'
#' It can also generate a DDI metadata file out of an R information object,
#' which can be used to export directly to the standard statistical packages
#' files (such as SPSS, Stata and SAS, or even Excel), using the versatile
#' package \bold{\pkg{haven}}. For R, the default object to store data and
#' metadata is a \bold{\code{data.frame}}, and labelled data are automatically
#' coerced to class \code{declared}.
#'
#' The research leading to the initial functions in this package has received
#' funding from the European Union's Seventh Framework Programme (FP7/2007-2013)
#' under grant agreement no. 262608 (DwB - Data without Boundaries)
#'
#' @author Adrian Dusa
#'
#' Maintainer: Adrian Dusa (dusa.adrian@unibuc.ro)
#'
#' @details
#' \tabular{ll}{
#'   Package: \tab DDIwR\cr
#'   Type: \tab Package\cr
#'   Version: \tab 0.14.2\cr
#'   Date: \tab 2023-05-19\cr
#'   License: \tab GPL-v3\cr
#' }
#'
#' @importFrom admisc anyTagged asNumeric getName numdec possibleNumeric
#' stopError trimstr tryCatchWEM wholeNumeric
#' @importFrom haven is_tagged_na is.labelled labelled labelled_spss na_tag
#' read_dta read_por read_sas read_sav read_xpt tagged_na write_dta write_sas
#' write_sav write_xpt
#' @importFrom declared as.declared as.haven declared is.declared undeclare
#' @importFrom tools file_ext
#' @importFrom xml2 as_list read_xml xml_attr xml_children xml_find_all
#' xml_find_first xml_name xml_ns xml_text
#' @importFrom readxl read_excel
#' @importFrom writexl write_xlsx
#' @importFrom utils file_test packageVersion read.csv write.table
#'
NULL
