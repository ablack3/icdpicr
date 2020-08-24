# create internal data for cat_trauma function
# read in lookup tables from the lookup tables folder and save add them to the package sysdata object

library(dplyr)

list.files("./lookup_tables")

sev_cc <- c("character", "integer", "character")


# icd 10 mapping to iss using Dave's roc max  method
i10base_map_roc <- read.csv("./lookup_tables/i10base_map_roc.csv",
                            stringsAsFactors = F,
                            colClasses = c("character", rep(c("integer", "character"), 4), rep("double", 4)))

i10cm_map_roc <- read.csv("./lookup_tables/i10cm_map_roc.csv",
                          stringsAsFactors = F,
                          colClasses = c("character", rep(c("integer", "character"), 4), rep("double", 4)))

# read in ICD 10 mappings using the GEM - two deduplication methods
i10_map_max <- read.csv("./lookup_tables/i10_map_max.csv", stringsAsFactors = F, colClasses = sev_cc)
i10_map_min <- read.csv("./lookup_tables/i10_map_min.csv", stringsAsFactors = F, colClasses = sev_cc)

# original i9 mapping with a few changes
ntab_s1 <- read.csv("./lookup_tables/ntab_s1.csv", stringsAsFactors = F, colClasses = sev_cc)

# i10_ecode (mechanism code) table created by Adam
i10_ecode <- read.csv("./lookup_tables/i10_ecode.csv", stringsAsFactors = F, colClasses = "character")

# Original ecode mapping changed to text instead of numeric codes
etab_s1 <- read.csv("./lookup_tables/etab_s1.csv", stringsAsFactors = F, colClasses = "character")

# check frequencies of issbr
library(purrr)
library(dplyr)
l <- lst(.select_i10_data("NIS", "cm"),
         .select_i10_data("NIS", "base"),
         .select_i10_data("TQIP", "cm"),
         .select_i10_data("TQIP", "base"),
         .select_i10_data("NIS_only", "cm"),
         .select_i10_data("NIS_only", "base"),
         .select_i10_data("TQIP_only", "cm"),
         .select_i10_data("TQIP_only", "base"),
         i10_map_max, i10_map_min, ntab_s1)
n <- names(l)
map2(l, n, ~count(.x, issbr, name = .y)) %>%
      reduce(full_join, by = "issbr")


# check col classes. These must ultimately be the same so they can be combined with rbind().

rbind(
      sapply(i10_ecode, class),
      sapply(etab_s1, class)
)

rbind(
      sapply(ntab_s1, class),
      sapply(i10_map_max, class),
      sapply(i10_map_min, class),
      sapply(.select_i10_data("NIS", "cm"), class),
      sapply(.select_i10_data("NIS", "base"), class),
      sapply(.select_i10_data("TQIP", "cm"), class),
      sapply(.select_i10_data("TQIP", "base"), class),
      sapply(.select_i10_data("NIS_only", "cm"), class),
      sapply(.select_i10_data("NIS_only", "base"), class),
      sapply(.select_i10_data("TQIP_only", "cm"), class),
      sapply(.select_i10_data("TQIP_only", "base"), class)
)

# col classes look good


# test binding of datasets
head(reduce(l, rbind))

head(rbind(ntab_s1, .select_i10_data("NIS", "cm")))
head(rbind(ntab_s1, .select_i10_data("NIS", "base")))
head(rbind(ntab_s1, .select_i10_data("TQIP", "cm")))
head(rbind(ntab_s1, .select_i10_data("TQIP", "base")))
head(rbind(ntab_s1, .select_i10_data("NIS_only", "cm")))
head(rbind(ntab_s1, .select_i10_data("NIS_only", "base")))
head(rbind(ntab_s1, .select_i10_data("TQIP_only", "cm")))
head(rbind(ntab_s1, .select_i10_data("TQIP_only", "base")))
head(rbind(ntab_s1, i10_map_min))
head(rbind(ntab_s1, i10_map_max))
head(rbind(etab_s1, i10_ecode))

# no errors

# create internal data
usethis::use_data(
      i10_map_min,
      i10_map_max,
      i10base_map_roc,
      i10cm_map_roc,
      ntab_s1,
      etab_s1,
      i10_ecode,
      internal = T, overwrite = T)


# add prelim directory to r build ignore
# usethis::use_build_ignore("prelim")

# add example data used for tests and examples
injury <- readr::read_csv("prelim/create_example_data/sample_data.csv",
                          col_types = paste(c(rep("c", 10), "i"), collapse = ""))


usethis::use_data(injury, overwrite = T)

