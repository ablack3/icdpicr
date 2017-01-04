# UNDER CONSTRUCTION

# ICDPICR 

International Classification of Diseases Programs for Injury Categorization in R

The ICDPICR package is an implementation of the STATA program ICDPIC version 3
in R. ICDPIC is a collection of Stata programs for injury categorization
and is available online at http://ideas.repec.org/c/boc/bocode/s457028.html

## Getting started

In order to download and use ICDPICR you will first need to install the free 
programming language R. This can be done by going to http://www.r-project.org and 
following the link to download R. Instructions and troubleshooting help is widely 
available online. 

We recommended that you also download and install the development environment for R called 
R Studio from http://www.rstudio.com/products/RStudio

Once these are installed and working properly you will need to run the following lines of code in R

install.packages("devtools")
devtools::install_github("ablack3/icdpicr")
library(icdpicr)

Access package help by running 
help(package = "icdpicr")

Run the trauma module on your data by first saving your data in CSV format
Then run the lines

df <- read.csv("C:/path/to/file.csv", stringsAsFactors = FALSE)
df_out <- trauma(df, "DX")

Save your output data with
write.csv(df_out, "C:/path/to/output.csv", row.names = FALSE)

Submit issues at http://github.com/ablack3/icdpicr/issues

