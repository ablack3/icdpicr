#' ICDPICR
#'
#' International Classification of Diseases Programs for Injury Categorization.
#' The ICDPICR package is an adaptation of the ICDPIC package originally written for Stata.
#' ICDPIC is a collection of Stata programs for injury categorization
#' and is available as a Stata download or online at \url{https://ideas.repec.org/c/boc/bocode/s457028.html}
#'
#' @section Version:
#' Version 0.1.0
#' ICDPICR is adapted from the ICDPIC Version 3.0 Stata program. Some of the
#' functionality of the original program has been reduced in the interest of providing updates for
#' only the most useful elements of the original ICDPIC program. ICDPICR is
#' designed to work with both ICD-9 and ICD-10 codes. ICDPICR is open
#' source and all code and documentation can be found at
#' \url{https://github.com/ablack3/icdpicr}.
#'
#'
#'
#' @section Description:
#' ICDPICR is an R package that currently consists of a single function that performs
#' the same task that the "trauma" program does in the Stata version of ICDPIC.
#' The intention of these ICDPIC programs
#' is to provide inexpensive methods for translating International
#' Classification of Diseases (ICD) diagnosis codes into standard injury
#' categories and/or scores.  Initial development of the ICDPIC Stata programs occurred
#' as part of research projects funded by the National Center for Injury
#' Prevention and Control through the Harvard Injury Control Research Center
#' (CDC R49/CCR 115279) and by the Maine Medical Center Research Strategic
#' Plan.  The translation of ICDPIC to R was supported by funding from the Maine Medical Center
#' Division of Trauma and Surgical Critical Care and Center for Outcomes Research and Evaluation.
#' The authors are grateful for this support and would also appreciate
#' suggestions or corrections from any user of the software. Bug reports or feature requests
#' may be submitted at \url{https://github.com/ablack3/icdpicr/issues}.
#' Publications of studies in which these programs or tables are used should cite the authors.
#'
#' @section Methods:
#' For each valid ICD-9-CM or ICD-10-CM injury diagnosis, ICDPIC-R is programmed to generate an approximate AIS
#' and body region, using the original AIS anatomic classification (as modified by Baker and colleagues) into
#' six body regions: Head and neck, face, chest, abdomen and pelvic contents, extremities and pelvic bones, and
#' general.  In addition, each code referring to a mechanism of injury is categorized as recommended or
#' proposed by the CDC. For each injured person, ICDPIC-R determines the maximal AIS in each body region
#' and overall, an Injury Severity Score (RISS), and a CDC mechanism category.
#'
#' Mapping of ICD-9-CM E-codes to CDC mechanism categories simply involved translation of the programming
#' code from Stata into R, using essentially the same table.  Mapping of ICD-10-CM codes to mechanism categories was
#' based on a similar table published by the CDC.
#'
#' The National Trauma Data Standard used by NTDB considers valid ICD-10-CM injury codes to be those in the
#' ranges S00-S99, T07, T14, T20-T28, and T30-32.  ICDPIC-R recognizes only these codes in the calculation
#' of injury severity from ICD-10, and also requires that the codes have a decimal point in the fourth position
#' and the letter "A" in the eighth position (indicating an initial encounter).
#' Mapping of ICD-10-CM codes to AIS severity is performed in two ways, as described below.
#'
#' \strong{“ROCmax” mapping method for ICD-10-CM codes:}
#' For each NTDB subject k with 1 to Nk valid ICD-10 codes,
#' each code was given a score of 0 if the subject survived and a score of 1/Nk
#' if the subject died (as recorded either in the Emergency Department file or in the Discharge file).
#' A fractional hospital mortality (HMF) was derived as the average score for each subject with a given
#' ICD-10 code.  Different cutpoints were investigated to assign each ICD-10 code to an AIS severity
#' based on HMF.  Body regions were assigned using the proposed CDC classification.
#' For each combination of cutpoints, the discrimination of the resulting ISS for predicting
#' mortality was determined using a C-statistic.
#' Among the combinations resulting in a maximum C-statistic (to two decimal places), the
#' combination was chosen that most closely approximated the ISS that had been calculated by
#' hospital trauma registrars (ISSAIS).
#'
#'
#' \strong{"GEM" mapping method for ICD-10-CM codes:}
#' ICD-10-CM codes are first mapped to ICD-9-CM codes using the General Equivalency Mapping (GEM)
#' tables provided by the Centers for Medicare and Medicaid Services (CMS), and then those ICD-9-CM
#' codes are mapped to AIS using the table inherited from the Stata version of ICDPIC.
#' The user is given the option to ignore ICD-10-CM codes if desired.  Otherwise, if the GEM maps an
#'  ICD-10-CM code to two or more ICD-9-CM codes associated with different severities, the user
#'  is given the option whether to assign the greater or lesser of these severities
#'  (“GEMmax” or “GEMmin”).  When the GEM maps an ICD-10-CM code to two or more ICD-9-CM codes
#'  associated with different AIS body regions, the verbal description of the ICD-10-CM code in the
#'  GEM table is used to assign a body region.
#'
#' The GEM mapping method is necessary when injuries have been coded with ICD-9-CM codes only,
#' and may be preferable when they have been coded with a mix of ICD-9-CM and ICD-10-CM.
#'
#'
#'
#' For any of the ICD-9 or ICD-10 mapping methods in ICDPIC-R, the maximum AIS Severity for each AIS body region
#' (MXAISBR1 ... MXAISBR6 in the output) is 0 if there are no valid injury codes for that body region.  It is
#' recorded as “missing” if there are valid codes for that body region, but their severity cannot be determined.
#'  Otherwise, it is the maximum known severity (1 through 6) for that body region.  Maximum AIS Severity
#'  (MAXAIS in the output data) is the maximum of (MXAISBR1 ... MXAISBR6); MAXAIS will thus be 0 if there is no
#'  diagnosis code associated with an AIS severity.
#'
#' Injury Severity Score (RISS) is calculated according to the classic description of Baker and colleagues,
#' namely the sum of the squares of the three largest elements of (MXAISBR1 ... MXAISBR6).  The user can choose
#' whether to assign RISS=75 when any injury is assigned a severity of 6, or to reassign a severity of 5 to
#' these injuries and calculate RISS as above.  The first option, RISS = 75 when any severity = 6, is the default
#' in ICDPIC-R, since by definition an AIS severity of 6 should denote an injury that is uniformly fatal and
#' thus should rarely be found in hospital data.
#'
#'
#' @section Functions:
#' \strong{cat_trauma} provides various classifications and characterizations of trauma
#' based on ICD-9-CM or ICD-10-CM diagnosis codes.
#'
#' @docType package
#' @name icdpicr
NULL
