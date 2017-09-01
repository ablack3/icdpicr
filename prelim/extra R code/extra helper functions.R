
#function that loads lookup tables
# load_lookup_tables <- function(){
#     library(purrr)
#     #lookup_tables <- c("cdtab_s1", "cdtab_s2", "cptab_s1", "cptab_s2", "drgtab_h", "etab_s1",  "etab_s2",
#      #                  "mais_s",   "micd9_s1", "micd9_s2", "ntab_s1",  "ntab_s2",  "xtab_s1h", "xtab_s2h")
#
#     lookup_tables <- c( "ntab_s1",  "ntab_s2")
#
#     lookup_table_filenames <- paste("./lookup tables/",lookup_tables,".csv", sep="")
#     map2(lookup_tables, lookup_table_filenames,
#          function(name, file) assign(name, read.csv(file, stringsAsFactors = F), envir = globalenv())
#          )
#
# }
#
# load_lookup_tables()

# load_lookup_tables <- function(){
#     assign("ntab_s1", read.csv("./lookup tables/ntab_s1.csv", stringsAsFactors = F), envir = globalenv())
#     assign("ntab_s2", read.csv("./lookup tables/ntab_s2.csv", stringsAsFactors = F), envir = globalenv())
# }

# # function to determine how many dx or px codes there are in the data
# .count_pre <- function(df, prefix){
#     #count number of columns with the prefix
#     count <- sum(grepl(paste("^",prefix,"[0-9]*$",sep=""), names(df)))
#     if(count < 1){
#         #warning("Prefix not found in dataset")
#         return(0)
#     }
#     # generate expected column names
#     col_names <- paste(prefix,1:count, sep="")
#     # check that expected column names are in the the dataset
#     if(all(col_names %in% names(df)) == F) {
#         warning(paste("dataset should contain column names:", paste(col_names,collapse = " ")))
#     }
#     count
# }



# ntab_s1 <- read.csv("./lookup tables/ntab_s1.csv", stringsAsFactors = F, colClasses = c("character","character","integer","integer","character"))
# etab_s1 <- read.csv("./lookup tables/etab_s1.csv", stringsAsFactors = F, colClasses = c("character","numeric","numeric","numeric"))
#
#



