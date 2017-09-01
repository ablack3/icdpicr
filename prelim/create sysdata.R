# create internal data for cat_trauma function
# read in lookup tables from the lookup tables folder and save add them to the package sysdata object

library(dplyr)
rm(list = ls())
list.files("./lookup_tables")

sev_cc <- c("character", "integer", "character")


# icd 10 mapping to iss using Dave's empirical method
i10_map_emp <- read.csv("./lookup_tables/i10_map_emp.csv", stringsAsFactors = F, colClasses = sev_cc)

# read in ICD 10 mappings using the GEM - two deduplication methods
i10_map_max <- read.csv("./lookup_tables/i10_map_max.csv", stringsAsFactors = F, colClasses = sev_cc)
i10_map_min <- read.csv("./lookup_tables/i10_map_min.csv", stringsAsFactors = F, colClasses = sev_cc)

# original i9 mapping with a few changes
ntab_s1 <- read.csv("./lookup_tables/ntab_s1.csv", stringsAsFactors = F, colClasses = sev_cc)

# i10_ecode (mechanism code) table created by Adam
i10_ecode <- read.csv("./lookup_tables/i10_ecode.csv", stringsAsFactors = F, colClasses = "character")

# Original ecode mapping changed to text instead of numeric codes
etab_s1 <- read.csv("./lookup_tables/etab_s1.csv", stringsAsFactors = F, colClasses = "character")


# check col classes. These must ultimately be the same so they can be combined with rbind().

rbind(
      sapply(i10_ecode, class),
      sapply(etab_s1, class)
)

rbind(
      sapply(ntab_s1, class),
      sapply(i10_map_max, class),
      sapply(i10_map_min, class),
      sapply(i10_map_emp, class)
)

# col classes look good


# test binding of datasets
head(rbind(ntab_s1, i10_map_emp))
head(rbind(ntab_s1, i10_map_min))
head(rbind(ntab_s1, i10_map_max))
head(rbind(etab_s1, i10_ecode))


# create internal data
devtools::use_data(
      i10_map_min,
      i10_map_max,
      i10_map_emp,
      ntab_s1,
      etab_s1,
      i10_ecode,
      internal = T, overwrite = T)


# note: the empirical mapping requires that trailling letters on icd10 codes are stripped off.
# Specifically, all characters after the first letter following the decimal should be dropped.
head(i10_map_emp)
# do I need to fix this?
# strip off trailing letters on all i10 codes?

# add prelim directory to r build ignore
devtools::use_build_ignore("prelim")

