#' ICDPICR
#'
#' International Classification of Diseases Programs for Injury Categorization
#' The ICDPICR package is an implementation of the STATA program ICDPIC version 3
#' in R. ICDPIC is a collection of Stata programs for injury categorization
#' and is available online at \url{https://ideas.repec.org/c/boc/bocode/s457028.html}
#'
#' @section Version:
#' Version 0.1.0
#' ICDPICR is adapted from the ICDPIC version 3.0 Stata code. However the
#' functionality has been decreased considerably in the interest of providing
#' only the most used elements of the original ICDPIC program. ICDPICR is open
#' source and all code and documentation can be found at
#' \url{http://github.com/ablack3/icdpicr}.
#'
#'
#'
#' @section Description:
#' ICDPICR is an R package that currently consists of a single function that performs
#' the same task that the trauma program does in ICDPIC.
#' The intention of ICDPIC described here
#' is to provide inexpensive methods for translating International
#' Classification of Diseases (ICD) diagnosis codes into standard injury
#' categories and/or scores.  Initial development of the ICDPIC Stata programs occurred
#' as part of research projects funded by the National Center for Injury
#' Prevention and Control through the Harvard Injury Control Research Center
#' (CDC R49/CCR 115279) and by the Maine Medical Center Research Strategic
#' Plan.  The translation of ICDPIC to R was supported by funding from ...
#' The authors are grateful for this support and would also appreciate
#' suggestions or corrections from any user of the software. Bug reports or feature requests
#' may be submitted at \url{http://github.com/ablack3/icdpicr/issues}.
#' Publications of
#' studies in which these programs or tables are used should cite the authors.
#' We hope ICDPIC will make ICD-9-CM codes easier to use for injury research,
#' and facilitate comparison of categorization methods.
#'
#' ICDPICR handles ICD-10-CM codes as well as ICD 9. This was accomplished by
#' first mapping ICD 10 codes to ICD 9 using the 2016 general equivalence mapping (GEM)
#' developed by CMS. Next the corresponding ICD 9 codes were mapped to anatomical
#' injury severity scores (ais) and body regions using the same mapping used in the
#' the original ICDPIC. In some cases an ICD 10 code was mapped to more than one
#' ICD 9 code. In these cases diambiguation is necessary to get a map from ICD 10
#' to AIS and body region.
#'
#' When an ICD 10 code was mapped to more than one body region a simple rule was
#' used based on keywords in the descriptions. Ambiguity involving ais scores
#' was handled by allowing the user to select the max ais or min ais associated with
#' the ICD 10 code.
#'
#' It is possible to tell the function to ignore ICD 10 codes if desired by the user.
#'
#' @section Functions:
#' \strong{trauma} provides various classifications and characterizations of trauma
#' based on ICD-9-CM diagnosis codes, specifically codes for Nature of Injury
#' (N-Codes) and External Cause of Injury (E-Codes).


#'
#' @docType package
#' @name icdpicr
NULL
