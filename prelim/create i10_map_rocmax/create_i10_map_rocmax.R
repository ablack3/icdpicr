#############################################################################################
# Dave worked out a new empirical method for assigning icd10 to iss using both the
# TQIP (Trauma Quality Improvement Program) and NIS (National Inpatient Sample) databases
# the performance of this method seems to be better than the GEM method and the previous
# empirical method. This methods was done using ridge regression and the full source
# code is inlcuded in the "prelim/buld ais mapping version 2" folder of this package.
# After each ICD code was ranked according to its contribution to morality the cutoffs
# used to assign the AIS were chosesn by maximizing the auc and is referred to the
# rocmax method

library(dplyr)

# The base table can be used for international ICD 10 codes
# The cm table can be used for US based ICD 10 CM codes
roc_base1 <- readr::read_csv("./prelim/create i10_map_rocmax/TQIP_NIS_ais_base_2021.csv")
roc_cm1 <-     readr::read_csv("./prelim/create i10_map_rocmax/TQIP_NIS_ais_cm_2021.csv")
head(roc_base1)
head(roc_cm1)


roc_cm1 %>%
   filter(icdcm %in% c(dx1 = "S00.81XA",
                       dx2 = "S22.081A", dx3 = "S22.41XA", dx4 = "S30.811A", dx5 = "S32.2XXA",
                       dx6 = "S42.251A", dx7 = "S42.291A", dx8 = "S82.231A", dx9 = "S82.491A"))


# is there any overlap in icd10 base and icd10cm?
codes_in_both <- intersect(roc_base1$icdbase, roc_cm1$icdcm)
# A small number of codes are in both tables

# look at distribution of code lengths
table(nchar(roc_base1$icdbase)) # mostly 5 digits
table(nchar(roc_cm1$icdcm)) # mostly 8 digits



roc_base <- roc_base1 %>%
      rename(dx = icdbase) %>%
      filter(dx != "T07.") %>% # remove code that ends with a .
      mutate(dx = stringr::str_remove(dx, "\\.")) %>%
      select(dx,
             TQIP_severity = TQIPais_mod,
             TQIP_issbr = TQIPbr_mod,
             NIS_severity = NISais_mod,
             NIS_issbr = NISbr_mod,
             TQIP_only_severity = TQIPais,
             TQIP_only_issbr = TQIPbr,
             NIS_only_severity = NISais,
             NIS_only_issbr = NISbr,
             TQIP_effect = TQIPeffect,
             TQIP_intercept = TQIPint,
             NIS_effect = NISeffect,
             NIS_intercept = NISint) %>%
      mutate_at(vars(contains("severity")), as.integer)


roc_cm <- roc_cm1 %>%
      rename(dx = icdcm) %>%
      filter(dx != "T07.") %>% # remove code that ends with a .
      mutate(dx = stringr::str_remove(dx, "\\.")) %>%
      select(dx,
             TQIP_severity = TQIPais_mod,
             TQIP_issbr = TQIPbr_mod,
             NIS_severity = NISais_mod,
             NIS_issbr = NISbr_mod,
             TQIP_only_severity = TQIPais,
             TQIP_only_issbr = TQIPbr,
             NIS_only_severity = NISais,
             NIS_only_issbr = NISbr,
             TQIP_effect = TQIPeffect,
             TQIP_intercept = TQIPint,
             NIS_effect = NISeffect,
             NIS_intercept = NISint) %>%
      mutate_at(vars(contains("severity")), as.integer)



# check that there are no duplicate codes
roc_base %>%
      add_count(dx) %>%
      filter(n > 1)

roc_cm %>%
      add_count(dx) %>%
      filter(n > 1)


# table to create
# i10cm_map_roc
# i10base_map_roc
# then do the variable mapping on the fly when the program runs


devtools::load_all()

# look at column types
sapply(roc_base, class)
# target to match
sapply(ntab_s1, class)

# check body region values
unique(ntab_s1$issbr)
roc_base %>%
      summarise_at(vars(matches("issbr")), ~paste(unique(.), collapse = "|")) %>%
      tidyr::gather()

roc_cm %>%
      summarise_at(vars(matches("issbr")), ~paste(unique(.), collapse = "|")) %>%
      tidyr::gather()



# final checks
# should be 1 to 6
unique(ntab_s1$severity)
roc_base %>%
      summarise_at(vars(matches("severity")), ~paste(unique(.), collapse = "|")) %>%
      tidyr::gather()

roc_cm %>%
      summarise_at(vars(matches("severity")), ~paste(unique(.), collapse = "|")) %>%
      tidyr::gather()

# save output table
write.csv(roc_cm, "./lookup_tables/i10cm_map_roc.csv", row.names = F)
write.csv(roc_base, "./lookup_tables/i10base_map_roc.csv", row.names = F)




