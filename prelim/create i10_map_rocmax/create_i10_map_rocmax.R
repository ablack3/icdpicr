#############################################################################################
# Dave worked out a second empirical method for assigning icd10 to iss using the NTDB data
# the performance of this method seems to be better than the GEM method.
# This method was created by maximizing the roc and is referred to the rocmax method

rm(list = ls())

# He gave me the results in an xls file.
roc <- readxl::read_excel("./prelim/create i10_map_rocmax/icd10ais_rocmax.xls")
head(roc)

# select only columns we use
roc <- subset(roc, select = c(icd_10, sev, issbr))

# look at column types
sapply(roc, class)
# target to match
sapply(ntab_s1, class)

# change severity to integer
roc$sev <- as.integer(roc$sev)

# now they match

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

# final checks
# should be 1 to 6
unique(roc$severity)
# should be char int char
sapply(roc, class)

head(roc)

# save output table
write.csv(roc, "./lookup_tables/i10_map_roc.csv", row.names = F)


