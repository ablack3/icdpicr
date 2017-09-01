# create i10 external cause of injury code to mechanism mapping
# generate lookup table from pdf using regex.
#

library(dplyr)
source("./prelim/mapping functions.R")

###################################   Main Program  #########################################
# read in the data
df <- read.csv("./prelim/create i10_ecode/icd10_transcode_in.csv", stringsAsFactors = F)
head(df)
# transpose to long format and split out intent codes
df2 <- tidyr::gather(df, key, value,-1:-3) %>%
      tidyr::separate(key, c("key", "intent"), -2) %>% mutate(intent = as.numeric(intent))

df3 <-  df2 %>%  mutate(key = substr(key, 1, nchar(key)-1))

#process each cell
df4 <- df3  %>% rowwise() %>%  mutate(codes = process_cell(value))
df5 <- splitstackshape::cSplit(df4, "codes", ",", type.convert = F)
purrr::dmap(df5, class) # make sure col classes are character
df6 <- df5 %>% select(-6) %>% tidyr::gather(col_number, dx, -1:-5) %>%
      select(-col_number) %>%
      filter(!is.na(dx))

# check that dx is unique id
nrow(df6)==length(unique(df6$dx))
# check duplicates if any
# df6 %>% group_by(dx) %>% mutate(n=n()) %>% filter(n>1)

# subset and map numeric codes to character values
final <- df6 %>% select(dx, mechmaj, mechmin, intent) %>% arrange(dx)


# replace e-code mech maj, mechmin and intent with text. Be sure mapping functions are loaded.
i10_ecode <- final %>%
      rowwise() %>%
      mutate(mechmin = map_mechmin(mechmaj, mechmin)) %>%
      mutate(mechmaj = map_mechmaj(mechmaj)) %>%
      mutate(intent = map_intent(intent))

write.csv(i10_ecode, file = "./lookup_tables/i10_ecode.csv", row.names = F)


###########################  end main program   #############################################


#############################################################################################
######### helper functions that need to loaded before running main program ##################
######### also some strings for testing. To understand read from bottom up ##################

library(stringr)

s <- "*U30-*U32 (.2;.4-.9)"
s <- "VU30-VU32 (.2;.4-.9)"
s <- "W25-W29"
s <- "V02-V04 (.1, .9)"
s <- " V09.2 "
process_unit(s)

cell <- "VU30-VU32 (.2;.4-.9), V34.0"
cell <- "V02-V04 (.1; .9)"
process_cell(cell)


process_cell <- function(cell){
      units <- str_split(cell,",") %>% unlist()
      res <- lapply(units, process_unit) %>% unlist()
      res <- paste(str_trim(res), collapse=",")
      res
}

# processes a unit after it has beeen split on commas
process_unit <- function(s){
      if(!str_detect(s, "\\(|-|\\)")){
            result <- str_replace(s, "\\.","")
      } else {

            # strip out components
            n <- str_extract_all(s, "[0-9][0-9]") %>% unlist()
            l <- str_extract(s, "[A-Z]") %>% unlist()
            parens <- str_extract(s, "\\(.*\\)") %>% unlist()

            #combine letter and double digit sequence
            nlist <- paste0(l, formatC(expand(n), width = 2, format="d", flag=0))

            if(!is.na(parens)){
                  # work on parens/ decimals
                  # split on semicolons
                  parens_split <- str_split(parens,";") %>% unlist()

                  # expand decimal lists
                  decimal <- sapply(parens_split, expand_parens) %>% unlist() %>% as.numeric()

                  # combine A12 A13 A14 with .2 .3 .4 -> A122 A123 ect..
                  result <- outer(nlist, decimal, paste, sep="") %>% as.vector() %>% sort()
            } else {
                  result <- nlist
            }

      }
      result
}

# extracts decimal values in parens after they have been split on ;
# expecting somthing like '(.2-.4' or '.2)'
expand_parens <- function(parens_split){
      n <- str_extract_all(parens_split, "[0-9]", simplify = T)
      expand(n)
}


# n is a lenth 1 or 2 character vector
# if length is 1 then the number is returned
# if length 2 then n[1]:n[2] is returned as numeric
expand <- function(n){
      # print(n)
      stopifnot(length(n) %in% c(1,2))
      if(length(n)==2){
            n2 <- seq(as.numeric(n[1]), as.numeric(n[2]), 1)
      } else if(length(n)==1){
            n2 <- as.numeric(n)
      }
      n2
}

