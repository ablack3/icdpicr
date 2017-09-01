# create test

library(readr)
library(foreign)
library(purrr)
library(dplyr)


# read in NHDS dataset
#setwd("C:\\Users\\awblack.MMCF\\Google Drive\\icdpic app")
setwd("/Users/adamblack/Google Drive/icdpic app")

varlengths <- read.csv("./nhds/varlengths.csv", stringsAsFactors = F)

nhds <- read_fwf("./nhds/NHDS_2010.PU.txt", fwf_widths(varlengths$len, col_names = varlengths$var))

#strip dashes
nhds <- dmap_at(nhds, c(paste("DX",0:15,sep=""), paste("PD",1:8,sep="")), ~gsub("-","",.x))

# remove spaces from var names
names(nhds) <- gsub(" ","",names(nhds))


#create test dataset
sample_size <- 15000
set.seed(1)
t1 <- nhds[sample(1:nrow(nhds), sample_size), ]

#stata version cant handle DX0 so we will drop it
t1 <- subset(t1, select = -DX0)

# select only important columns and add ID
nhds_test1 <- t1 %>% select(dplyr::matches("^DX|^PD")) %>% mutate(ID = 1:nrow(.)) %>% select(ID, everything())



# save as stata dataset
write.dta(nhds_test1, "./test data/NHDS_ss.dta")

#########################
## run icdpic in stata ##
#########################


# import results
# method 1
t1s_m1 <- read.dta("./test data/NHDS_ss_out_m1.dta", convert.underscore = F)
# method 2
t1s_m2 <- read.dta("./test data/NHDS_ss_out_m2.dta", convert.underscore = F)
names(t1s_m2)

#convert logical columns (all NA) to character
for(i in 1:ncol(t1s_m1)) if(class(t1s_m1[ ,i]) == "logical") t1s_m1[ ,i] <- as.character(t1s_m1[ ,i])
for(i in 1:ncol(t1s_m2)) if(class(t1s_m2[ ,i]) == "logical") t1s_m2[ ,i] <- as.character(t1s_m2[ ,i])
#t1s2 <- dmap(t1s, ~ifelse(class(.)=="logical", as.character(.),.))

#convert integer columns to numeric
for(i in 1:ncol(t1s_m1)) if(class(t1s_m1[ ,i])=="integer") t1s_m1[ ,i] <- as.numeric(t1s_m1[ ,i])
for(i in 1:ncol(t1s_m2)) if(class(t1s_m2[ ,i])=="integer") t1s_m2[ ,i] <- as.numeric(t1s_m2[ ,i])
#t1s <- dmap(t1s, ~ifelse(class(.)=="integer", as.numeric(.),.))
#t1r <- dmap(t1r, ~ifelse(class(.)=="integer", as.numeric(.),.))

t1s_m1 <- t1s_m1 %>% select(-dplyr::contains("apc_")) %>% select(-dplyr::contains("brl_"))
t1s_m2 <- t1s_m2 %>% select(-dplyr::contains("apc_")) %>% select(-dplyr::contains("brl_"))

#convert "" and "NA" to NA
ccs <- map_chr(t1s_m1,class)
charcols <- names(ccs[ccs=="character"])
t1s_m1 <- dmap_at(t1s_m1, charcols, ~ifelse(.x %in% c("", "NA"), NA, .x))

ccs <- map_chr(t1s_m2,class)
charcols <- names(ccs[ccs=="character"])
t1s_m2 <- dmap_at(t1s_m2, charcols, ~ifelse(.x %in% c("", "NA"), NA, .x))

# input data
# nhds_test1

# expected output data for method 1
# t1s_m1

# expected output data for method 2
# t1s_m2


############################ run unit test ##################################

t1r_m1 <- trauma(nhds_test1, "DX", calc_method = 1, icd10 = F)
t1r_m2 <- trauma(nhds_test1, "DX", calc_method = 2, icd10 = F)


#convert logical columns (all NA) to character
for(i in 1:ncol(t1r_m1)) if(class(t1r_m1[ ,i]) == "logical") t1r_m1[ ,i] <- as.character(t1r_m1[ ,i])
for(i in 1:ncol(t1r_m2)) if(class(t1r_m2[ ,i]) == "logical") t1r_m2[ ,i] <- as.character(t1r_m2[ ,i])


#convert integer columns to numeric
for(i in 1:ncol(t1r_m1)) if(class(t1r_m1[ ,i])=="integer") t1r_m1[ ,i] <- as.numeric(t1r_m1[ ,i])
for(i in 1:ncol(t1r_m2)) if(class(t1r_m2[ ,i])=="integer") t1r_m2[ ,i] <- as.numeric(t1r_m2[ ,i])


data_frame(r=names(t1r_m1), s = names(t1s_m1)) %>% mutate(eq = (r==s))
cbind(names(t1r_m2),names(t1s_m2))

library(testthat)

expect_equal(class(t1r_m1), class(t1s_m1))
expect_equal(names(t1r_m1), names(t1s_m1))
expect_equal(rownames(t1r_m1), rownames(t1s_m1))
expect_equivalent(t1r_m1, t1s_m1)

expect_equal(class(t1r_m2), class(t1s_m2))
expect_equal(names(t1r_m2), names(t1s_m2))
expect_equal(rownames(t1r_m2), rownames(t1s_m2))
expect_equivalent(t1r_m2, t1s_m2)
