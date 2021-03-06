# Test

library(dplyr)
set.seed(1)


############### test e code mapping ######################################
# create a 5x5 dataset from random codes in our lookup tables. all e-codes.
size <- c(6,5)
dx_codes <- sample(rbind(etab_s1, i10_ecode)$dx, size[1]*size[2])
df <- matrix(dx_codes, nrow = size[1], ncol = size[2]) %>% as.data.frame()
names(df) <- paste0("DX", 1:size[2])

df <- rbind(df, rep(NA, size[2]))
df

res <- cat_trauma(df,"DX")
res %>% select(matches("DX|ecode|mech|intent"))
names(res)

colClasses <- function(df) apply(df, 2, class)
colClasses(df)
colClasses(res)

colClasses(rbind(etab_s1, i10_ecode))




###################### test help page ######################
help(package="icdpicr")
?icdpicr::cat_trauma


# example
df_in <- read.table(header = T, text = "
      ident    dx1     dx2     dx3
      31416   800.1   959.9   E910.9
      31417   800.24  410.0   NA
")
df_out <- cat_trauma(df_in, "dx")
df_out


df_in <- read.table(header = T, text = "
ident    dx1     dx2     dx3
                    31416   800.1   959.9   E910.9
                    31417   800.24  410.0   NA
                    ")
df_out <- cat_trauma(df_in, "dx")
df_out

library(icdpicr)
library(dplyr)
data.in <- injury %>%
      head(8) %>%
      # select(matches("dx")) %>%
      mutate_all(substr, 1, 5)

cat_trauma(data.in, "dx", icd10 = "base", i10_iss_method = "roc_max_NIS")

df = data.in
dx_pre =  "dx"; icd10 = "base"; i10_iss_method = "roc_max_NIS"; calc_method = 1
