#############################################################################################
# Dave worked out a second rocirical method for assigning icd10 to iss using the NTDB data
# the performance of this method seems to be better than the GEM method.
# This method was created by maximizing the roc

rm(list = ls())

# He gave me the results in an xls file.
roc <- read.csv("./prelim/create i10_map_rocmax/icd10ais_rocmax.csv", stringsAsFactors = F)
head(roc)

# select only columns we use
roc <- subset(roc, select = c(icd_10, sev, issbr))

# look at column types
sapply(roc, class)
# target to match
sapply(ntab_s1, class)

# they match

# rename columns
names(roc) <- c("dx", "severity", "issbr")

# remove duplicate rows
nrow(roc)
# 4998
roc <- unique(roc)
nrow(roc)
# 4924


# strip decimal
roc$dx <- sub("\\.", "", roc$dx)

# note that body region is text in this table.
unique(roc$issbr)

# final check
unique(roc$severity)
sapply(roc, class)
head(roc)

# save output table
write.csv(roc, "./lookup_tables/i10_map_roc.csv", row.names = F)


