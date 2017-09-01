# Program: create icd 10 to AIS and body region mapping
# adam black awblack@mmc.org
# purpose: use the CMS 2016 general equivalence mapping (GEM) to create a mapping from ICD10 to ISS
# date: dec 23, 2016
# reworked: Aug 26, 2017
# approach: investigate the magnitude and nature of the one to many mappings (refered to as gem conflicts)
#           Then resolve body region conflicts using a simple rule based on keywords in the code descriptions
#           The remaining ISS conflicts will be resolved by taking either the max or min ISS for each ICD10 code
#           This decision will be punted to the user at runtime.
#
# output is two tables
# i10_map_max.csv and i10_map_min.csv in the lookup tables folder
# these map icd10 codes to severity and body region.

library(dplyr)
library(readr)
library(purrr)
source("./prelim/mapping functions.R")

# gem file downloaded from https://www.cms.gov/medicare/coding/icd10/2016-icd-10-cm-and-gems.html

gem_file <- "./prelim/create i10_map_min i10_map_max/GEM2016/2016-General-Equivalence-Mappings/2016_I10gem.txt"
gem <- read.fwf(gem_file,
                widths = c(8,6,5), col.names = c("i10","i9","code"),
                colClasses="character", strip.white=T)

# ecode to mechanism mapping from original icdpic stata app
# etab_s1 <- read.csv("./prelim/original icdpic lookup tables/etab_s1.csv", stringsAsFactors = F,
#                     colClasses = c("character","numeric","numeric","numeric"))

# read in code descriptions
i10_desc <- read.fwf("./prelim/create i10_map_min i10_map_max/GEM2016/2016-Code-Descriptions-in-Tabular-Order/icd10cm_codes_2016.txt",
                   widths = c(7,500), col.names = c("i10","i10_desc"), colClasses="character", strip.white=T)

i9_desc <- read_csv("./prelim/create i10_map_min i10_map_max/GEM2016/ICD-9-CM-v32-master-descriptions/CMS32_DESC_LONG_SHORT_DX.csv", col_types = "ccc")[,1:2]
names(i9_desc) <- c("i9", "i9_desc")
head(i9_desc)

# one to many counts in GEM
gem %>% group_by(i10) %>% summarise(count=n()) %>% group_by(count) %>% summarise(n=n())

# merge with ntab
ntab_s1 <- read.csv("./lookup_tables/ntab_s1.csv", stringsAsFactors = F, colClasses = c("character","integer","character"))
gem2 <- inner_join(gem, ntab_s1, by=c("i9"="dx"))

names(gem2)

gem2 <- gem2 %>%
      mutate(vals = paste(severity, issbr)) %>%
      group_by(i10) %>%
      mutate(n_unique = length(unique(vals)),
             n_unique_br = length(unique(issbr)),
             n_unique_sev = length(unique(severity))) %>%
      ungroup()

names(gem2)

length(unique(gem2$i10))
# 9392 ICD 10 codes

# create table data (td) with one row per icd10 code and the number of unique iss and body regions it is mapped to
td <- gem2 %>%
      group_by(i10) %>%
      summarise(n_unique = max(n_unique),
                n_unique_br = max(n_unique_br),
                n_unique_sev = max(n_unique_sev))


# how many are only mapped to >1 combination of severity and body region?
rbind(count = table(td$n_unique), percent = round(table(td$n_unique)/nrow(td)*100, 1))
# 251/9392 = 2.7% of i10 codes are mapped to >1 iss or body region


# how many are only mapped to >1 solutions for severity only?
rbind(count = table(td$n_unique_sev), percent = round(table(td$n_unique_sev)/nrow(td)*100, 1))
# 229/9392 = 2.5% of i10 codes are mapped to >1 iss or body region


# how many are only mapped to >1 body region?
rbind(count = table(td$n_unique_br), percent = round(table(td$n_unique_br)/nrow(td)*100, 1))
# 47/9392 = .5% of i10 codes are mapped to >1 body region

# we will make the user decide how to disambiguate i10 to severity mapping using max or min
# however we need a rule to map i10 to body region
# gemconflicts is just the icd10 mappings with body region conflicts
gemconflicts <- gem2 %>%
      filter(n_unique_br > 1) %>%
      select(-vals, -code) %>%
      left_join(i10_desc, by = "i10") %>%
      left_join(i9_desc, by = "i9")

# sent csv file to Dave for ideas on how to assign body region
# write_csv(gemconflicts,"./GEM conflicts.csv")

# how many i10 codes do we need to map manually?
length(unique(gemconflicts$i10))
# 47 codes



# manually assign body region when there is a comflict using this rule:

# Assign body region 1 (head and neck) if the field "i10_desc" contains the word "head".
# Assign body region 2 (face) if the field "i10_desc" contains the words "ear" or "eye".
# Assign body region 3 (chest) if the field "i10_desc" contains the word "thorax".
# Assign body region 5 (extremities and pelvic girdle) if the field "i10_desc" contains the words "pelvis",
# "lower limb", "thigh", "knee", "lower leg", "ankle", "foot", "toe", "upper limb", "upper arm", "elbow",
# "forearm", "wrist", "hand", "palm", "finger", or "thumb".

map_br <- function(desc){
      br <- 0
      if(grepl("head",desc)){ br <- 1}
      else if(grepl(" ear| eye",desc)){ br <- 2}
      else if(grepl("thorax", desc)){ br <- 3}
      else if(grepl("pelvis|lower limb|thigh|knee|lower leg|ankle|foot|toe|upper limb|upper arm|elbow|forearm|wrist|hand|palm|finger|thumb", desc)){ br <- 5}
      br
}

# test mapping function
map_br("lower limb")

# assign body region based on rule
gemconflicts$new_br <- sapply(gemconflicts$i10_desc, map_br)

# add new body region to the gem
gem2 <- left_join(gem2, gemconflicts[ , c("i10", "new_br")], by = "i10")

sum(!is.na(gem2$new_br))
# 373 rows in the GEM have non-missing new body region values

# just check that there are only 47 codes with non missing new body region
gem2 %>%
      filter(!is.na(new_br)) %>%
      .[["i10"]] %>%
      unique() %>%
      length()


# only use new body region if there is a conflict
gem2 <- gem2 %>% mutate(new_br2 = ifelse(is.na(new_br), issbr, new_br))


# take a look at the gem mapping codes.
# this code describes the nature of the mapping. details below.
table(gem2$code)
# code 10000 means approximate single code mapping
# might be one to many but we know it is not a composite mapping
# code 10111 means approximate, has a match, a composite match, group 1, element 1
# the other elements must not be in the ntab file so we will just use the iss on the one that is.
# This is just a check and I dont think it requres any action.

# create the mapping
gem3 <- gem2 %>%
      select(i10, severity, new_br2) %>%
      rename(issbr=new_br2) %>%
      distinct()

# filter only codes that are 7 characters long and have an "A" in the 7th position (first encounter)
gem3 <- gem3 %>%
      filter(nchar(i10) == 7) %>%
      filter(substr(i10,7,7) == "A")

# check one to many mappings
tst <- gem3 %>%
      group_by(i10) %>%
      mutate(n_unique_iss = length(unique(severity))) %>%
      mutate(n_unique_br = length(unique(issbr))) %>%
      summarise(n_unique_iss = max(n_unique_iss),
                n_unique_br = max(n_unique_br))

table(tst$n_unique_br)
# all i10 codes are now mapped to a single body region

table(tst$n_unique_iss)
# there are 215 i10 codes mapped to 2 iss
# and 5 codes mapped to 3 iss scores


# we will give the user the option to choose min or max iss to resolve the severity conflicts



gem3_max <- gem3 %>%
      group_by(i10) %>%
      summarise(severity = max(severity),
                issbr = max(issbr))

gem3_min <- gem3 %>%
      group_by(i10) %>%
      summarise(severity = min(severity),
                issbr = min(issbr))

# check that max and min methods agree on body region
left_join(gem3_max, gem3_min, by = "i10") %>%
      mutate(br_agree = (issbr.x == issbr.y)) %>%
      .[["br_agree"]] %>%
      all()
# yes they do

# check that 220 i10 codes have different severity scores
left_join(gem3_max, gem3_min, by = "i10") %>%
      mutate(sev_disagree = (severity.x != severity.y)) %>%
      .[["sev_disagree"]] %>%
      sum()
# yes



# check that there is only one row for each i10 score
length(unique(gem3_max$i10)) == nrow(gem3_max)
length(unique(gem3_min$i10)) == nrow(gem3_min)
# 9392 i10 codes

# rename columns
names(gem3_max)[1] <- "dx"
names(gem3_min)[1] <- "dx"
names(gem3_max)
names(gem3_min)
# "dx"       "severity" "issbr"

# change severity = 9 (unknown) to NA
gem3_max$severity <- ifelse(gem3_max$severity == 9, NA, gem3_max$severity)
gem3_min$severity <- ifelse(gem3_min$severity == 9, NA, gem3_min$severity)

# check
unique(gem3_max$severity)
unique(gem3_min$severity)

# output
write_csv(gem3_min, "./lookup_tables/i10_map_min.csv")
write_csv(gem3_max, "./lookup_tables/i10_map_max.csv")

