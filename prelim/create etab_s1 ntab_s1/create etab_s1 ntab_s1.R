# create the updated etab_s1 and ntab_s1 tables
# save the output in the lookup tables folder as csv files

library(dplyr)
source("./prelim/mapping functions.R")

# icd9 to severity table from original icdpic stata program
ntab_s1 <- read.csv("./prelim/create etab_s1 ntab_s1/original icdpic lookup tables/ntab_s1.csv", stringsAsFactors = F)

# select only the columns we will use
ntab_s1 <- ntab_s1[ , c("dx","severity","issbr")]

# make corrections suggested by Fleischman et al.

# ICD9 850.11 should be severity 2 not 1
ntab_s1[which(ntab_s1$dx == 85011), "severity"] <- 2
# ICD9 862.8 should be severity 5 not 6
ntab_s1[which(ntab_s1$dx == 8628), "severity"] <- 5

# convert issbr to character : make sure map_issbr function is loaded.
ntab_s1 <- ntab_s1 %>% rowwise() %>% mutate(issbr2 = map_issbr(issbr))

# check conversion
ntab_s1 %>% select(starts_with("issbr")) %>% unique()

# drop original and convert to dataframe (not tibble)
ntab_s1 <- ntab_s1 %>% select(-issbr) %>% rename(issbr = issbr2) %>%  as.data.frame()

# convert dx to character
ntab_s1$dx <- as.character(ntab_s1$dx)

# final checks
sapply(ntab_s1, class)
head(ntab_s1)


# save lookup tables as csv
write.csv(ntab_s1, file = "./lookup_tables/ntab_s1.csv", row.names = F)



###################

# original icdpic icd9 e-code mapping table
etab_s1 <- read.csv("./prelim/create etab_s1 ntab_s1/original icdpic lookup tables/etab_s1.csv", stringsAsFactors = F)

etab_s1 <- etab_s1 %>%
      rowwise() %>%
      mutate(mech_min = map_mechmin(mechmaj, mechmin)) %>%
      mutate(mechmaj = map_mechmaj(mechmaj)) %>%
      mutate(intent = map_intent(intent)) %>%
      as.data.frame()

# chech that mapping is correct

# drop numeric columns
etab_s1 <- etab_s1 %>%
      select(-mechmin) %>%
      rename(mechmin = mech_min) %>%
      select(dx, mechmaj, mechmin, intent)


# checks
head(etab_s1,2)
sapply(etab_s1, class)

write.csv(etab_s1, file = "./lookup_tables/etab_s1.csv", row.names = F)

