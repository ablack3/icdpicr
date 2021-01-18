#############################################################################################
# Dave worked out an empirical method for assigning icd10 to iss using the NTDB data
# the performance of this method seems to be better than the GEM method.
# He gave me the results in an xls file.
emp <- read.csv("./prelim/create i10_map_emp/icd10aisnew.csv", stringsAsFactors = F)

# look at column types
sapply(ntab_s1,class)

# select only columns we use
emp <- subset(emp, select = c(icd_10, sev, issbr))

# rename columns
names(emp) <- c("dx", "severity", "issbr")

# remove duplicate rows
nrow(emp)
# 4998
emp <- unique(emp)
nrow(emp)
# 4924


# strip decimal
emp$dx <- sub("\\.", "", emp$dx)

# body region is text in this table.
# in the next program, create sysdata.R,
# I will convert the other lookup tables' body region colums to text as well.
unique(emp$issbr)

# check
unique(emp$severity)
sapply(emp, class)
head(emp)

# save output table
write.csv(emp, "./lookup_tables/i10_map_emp.csv", row.names = F)


