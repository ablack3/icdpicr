# ICDPICR 

International Classification of Diseases Programs for Injury Categorization in R

The ICDPICR package is an implementation of the STATA program ICDPIC version 3
in R. ICDPIC is a collection of Stata programs for injury categorization
and is available online at https://ideas.repec.org/c/boc/bocode/s457028.html

A paper with a full description of ICDPIC-R can be found here: https://rdcu.be/KZM7 

A web application version of ICDPIC-R which does not require any installation can be found here: https://ablack3.shinyapps.io/icdpicr_app/

## Getting started

In order to download and use ICDPICR you will first need to install the free 
programming language R. This can be done by going to http://www.r-project.org and 
following the link to download R. Instructions and troubleshooting help is widely 
available online. 

We recommended that you also download and install the development environment for R called 
R Studio from http://www.rstudio.com/products/RStudio

Once these are installed and working properly you will need to run the following lines of code in R

```R
install.packages("devtools")
devtools::install_github("ablack3/icdpicr")
library(icdpicr)
```

Access package help by running 
```R
help(package = "icdpicr")
```
Categorize trauma ICD codes in your data by first saving your data in CSV format. The data should be in *wide* format with one row per observation and one column per ICD code. ICD code columns need to have a common prefix (e.g. "dx1", "dx2", "dx3", etc.)



Then run the lines
```R
df <- read.csv("C:/path/to/file.csv", stringsAsFactors = FALSE)
df_out <- cat_trauma(df, dx_pre = "dx", icd10 = FALSE) # use only ICD-9 codes
```                           
Save your output data with
```R
write.csv(df_out, "C:/path/to/output.csv", row.names = FALSE)
```

If your data has ICD-10-CM codes please read over the package documentation to choose the correct settings for your data.

Submit issues at http://github.com/ablack3/icdpicr/issues

