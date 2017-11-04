#' Categorize trauma by adding AIS and ISS to a dataframe
#'
#' For each observation this function will
#' \enumerate{
#'    \item assign a severity (AIS) and ISS body region values to each valid ICD-9-CM or ICD-10-CM trauma code,
#'    \item add variables for maximum severity of each body region,
#'    \item calculate ISS
#'    \item select first 4 e-codes/mechanism codes along with major mechanism, minor mechanism, and intent
#'}
#'
#'
#'
#'
#' @param df A dataframe in wide format containing ICD-9 and/or ICD-10 diagnosis codes with a common column name prefix.
#'          Diagnosis codes should be character strings and may have a decimal or not.
#'
#' @param dx_pre Prefix for diagnosis code column names (example: dx1, dx2, etc.)
#'
#' @param calc_method ISS calculation method:
#'          Method 1 (default) will assign an ISS of 75 if any AIS is 6.
#'          Method 2 will change any AIS = 6 to 5 and then calculate ISS normally.
#'
#' @param icd10 A logical value indicating whether ICD-10 codes should be considered or ignored.
#'          If TRUE (default) then ICD-10 codes are handled based on the i10_iss_method argument
#'          and ICD-10 mechanism codes will be included in E-code calculation.
#'          If FALSE then ICD-10 codes are ignored.
#'
#' @param i10_iss_method Method for calculating ISS from ICD10-CM codes. Must be one of:
#'          \itemize{
#'          \item "roc_max" (default) Table derived empirically from National Trauma Data Bank using ROC c-stat as objective. Details are included in ICDPIC-R package help documentation.
#'          \item "gem_max" Table derived by mapping ICD 10 to ICD 9 using the CMS general equivalence mapping tables and then to ISS
#'                 using the original ICDPIC table. Mapping conflicts handled by taking the max ISS.
#'          \item "gem_min" Same as "gem_max" except that mapping conflicts are handled by taking the min ISS.
#'          }
#'
#' @return A dataframe identical to the dataframe passed to the function with the following additional variables
#'          added:
#'          \itemize{
#'          \item sev_1-sev_n: AIS severity for diagnosis codes 1..n
#'          \item issbr_1-issbr_n: ISS body region for diagnosis codes 1..n
#'          \item mxaisbr1-mxaisbr6: maximum AIS severity for each of the 6 ISS body regions
#'          \item maxais: maximum AIS severity over all ISS body regions
#'          \item riss: computed injury severity score
#'          \item ecode_1-ecode_4: first 4 mechanism/E-Codes (including ICD10 if requested) found in each row of data
#'          \item mechmaj1-mechmaj4: CDC external cause of injury major mechanism for each E-Code captured
#'          \item mechmin1-mechmin4: CDC external cause of injury minor mechanism for each E-Code captured
#'          \item intent1-intent4: intent for each E-Code captured
#'          \item lowmech: lowest CDC external cause of injury major mechanism for all E-Codes captured
#'          }
#'
#' @details  Data should be in wide format:
#' \tabular{rrrr}{
#' ID  \tab  dx1 \tab  dx2 \tab dx3 \cr
#' 31416 \tab   800.1 \tab   959.9 \tab   E910.9 \cr
#' 31417  \tab 800.24  \tab 410.0 \tab
#' }
#'
#' Codes for AIS severity:
#' \itemize{
#'       \item 1 = Minor
#'       \item 2 = Moderate
#'       \item 3 = Serious
#'       \item 4 = Severe
#'       \item 5 = Critical
#'       \item 6 = Unsurvivable
#'       \item 9 = Unknown
#'}
#'
#' @examples df_in <- read.table(header = T, text = "
#' ident    dx1     dx2     dx3
#' 31416   800.1   959.9   E910.9
#' 31417   800.24  410.0   NA
#' ")
#' df_out <- cat_trauma(df_in, "dx")
#'
#' @export




# for debuging...
# set.seed(1)
# codes <- c()
# n <- 5
# df <- data.frame(dx1 = sample(ntab_s1$dx, n),
#                  dx2 = sample(ntab_s1$dx, n),
#                  dx3 = sample(i10_map_emp$dx, n),
#                  dx4 = sample(etab_s1$dx, n),
#                  dx5 = sample(i10_ecode$dx, n))
#
# result <- cat_trauma(df,"dx")
#
# df
# dx_pre="dx"
# calc_method = 1
# icd10 <- T
# i10_iss_method <- "empirical"

cat_trauma <- function(df, dx_pre, calc_method = 1, icd10 = TRUE, i10_iss_method = "roc_max"){

      # Verify input #
      if(!is.data.frame(df)) stop("First argument must be a dataframe")
      if(NROW(df) == 0) stop("Data contains no observations.")
      if(!is.character(dx_pre)) stop("Second argument must be a character string")
      # ensure dx_pre is a valid variable name
      if(make.names(dx_pre) != dx_pre) stop("Second argument must be a valid variable name in R")
      if(!(calc_method %in% c(1,2))) stop("calc_method must be either 1 or 2")
      if(!(icd10 %in% c(T, F))) stop("icd10 must be TRUE or FALSE")
      if(!(i10_iss_method %in% c("empirical","roc_max","gem_max","gem_min"))) stop("i10_iss_menthod must be empirical, roc_max, gem_max, or gem_min")

      # Check if user entered a correct prefix for the diagnosis code variables in the input file
      # Determine how many diagnosis code variables there are in the data
      regex_dx <- paste0("^", dx_pre, "([0-9]+)$")
      dx_colnames <- grep(regex_dx, names(df), value = T)
      # replace full column name with first capture group and convert to number
      dx_nums <- as.numeric(sub(regex_dx, "\\1", dx_colnames))
      num_dx <- length(dx_nums)
      if(num_dx == 0) stop("No variables with prefix found in data")

      # make sure df is not a tibble and if it is convert back to regular dataframe
      df <- data.frame(df)

      # add i10 codes to lookup tables
      # The i10 mappings  for n codes were created by using both CMS general equivalence mappings
      # and Dave's empirical method
      # The i10 maapings for e codes (mechanism) were created using CDC injury mechanism grid
      # See documentation and prelim directory for details
      if(icd10){
            etab <- rbind(etab_s1, i10_ecode)

            if(i10_iss_method == "roc_max"){
                  ntab <- rbind(ntab_s1, i10_map_roc)
            } else if(i10_iss_method == "gem_max"){
                  ntab <- rbind(ntab_s1, i10_map_max)
            } else if(i10_iss_method == "gem_min"){
                  ntab <- rbind(ntab_s1, i10_map_min)
            }
      } else {
            ntab <- ntab_s1
            etab <- etab_s1
      }

      #---------------------------------------------------------------------------------#
      #  Merge diagnosis code variables with N-Code reference table to obtain severity  #
      #  and ISS body region variables for each diagnosis code and add them to the data #
      #---------------------------------------------------------------------------------#
      message("inserting severity and body_region columns")
      for(i in dx_nums){
          # create column name
          dx_name <- paste0(dx_pre, i)

          # pull just the diagnosis code column of interest
          df_ss <- df[ , dx_name, drop=F]

          # add row variable for sorting back to original order
          df_ss$n <- 1:NROW(df_ss)

          # strip out decimal in all codes
          df_ss[ , dx_name] <- sub("\\.", "", df_ss[ , dx_name])

          # For ICD 9 codes we do not neet to check the format since only valid codes will be matched
          # when we merge in the ISS from the lookup tables
          # Note that this assumes that no ICD 10 codes will inadvertently be matched to ICD 9 codes
          # I think this is true but am not 100% sure yet.
          # OK... V codes are a problem. V12 is both a valid I9 and I10 code for example
          # E codes are also a problem if we strip the decimal. E800 after decimal stripping
          # is in both I9 and I10 (E80.0).

          # Luckily we are only dealing with a subset of I9 and I10
          # for the N-codes
          # The I9 subset only include codes that start with 8 or 9
          # The I10 subset only includes codes that start with S or T

          # for the E codes (nature of injury codes)
          # I10 start with "U" "V" "W" "X" "Y"
          # I9 start with "E"

          # is it possible that an I9 V code gets classified as I10?
          # Yep seems so. V20 will be matched with I10 even though it could be an I9 code.

          # The same is not true of the ICD 10 codes since there are placeholder "X" characters
          # allowed. If the user requests ICD 10 then we need to validate ICD 10 codes and
          # the process them according to what Dave did when he created the empirical table.

          # The National Trauma Data Standard used by NTDB considers valid ICD-10-CM injury
          # codes to be those in the ranges S00-S99, T07, T14, T20-T28, and T30-32.
          # ICDPIC-R recognizes only these codes in the calculation of injury severity from ICD-10,
          # and also requires that the codes have a decimal point in the fourth position and the
          # letter “A” in the eighth position (indicating an initial encounter).

          # we only need to do this processing for the roc_max method
          # if using the Gem then the code validation is automatically handled through the merge just like in
          # the icd9 case
          if(icd10 == TRUE & i10_iss_method == "roc_max"){

              i9_valid <- c("8","9","E")
              i10_valid <- c("S","T","U","V","W","X","Y")

              # get rid of codes that do not start with a valid character
              df_ss[ , dx_name] <- ifelse(substr(df_ss[,dx_name],1,1) %in% c(i9_valid, i10_valid), df_ss[,dx_name], NA)

              # any codes starting with V are assumed to be ICD 10
              # if the code is I9 (starts with 8, 9, or E) then leave it alone
              # otherwise

              # if the code starts with "S","T","U","V","W","X","Y" then process by

                  # checking that 7th (last) character is an A and then stripping it off
                  # stripping the first X found and any characters after it

              process_i10 <- function(s){
                  stopifnot(is.character(s) | is.na(s))
                  ret_val <- NA
                  s <- sub("\\.", "", s)
                  if(!substr(s,1,1) %in% c("S","T","U","V","W","X","Y")) ret_val <- s
                  else if(nchar(s) != 7) ret_val <- ""
                  else if(substr(s,7,7) != "A") ret_val <- ""
                  else if(substr(s,5,5) == "X") ret_val <- substr(s,1,4)
                  else if(substr(s,6,6) == "X") ret_val <- substr(s,1,5)
                  else ret_val <- substr(s,1,6)
                  return(ret_val)
              }

              # process the codes
              df_ss[ , dx_name] <- sapply(df_ss[ , dx_name], process_i10)

              # tst$dx12 <- sapply(tst$dx1, process_i10)
              # process_i10("S80.812A")
              # unique(substr(i10_map_emp$dx,1,3))
              # unique(substr(i10_map_max$dx,1,3))
          }


          temp <- merge(df_ss, ntab, by.x=dx_name, by.y="dx", all.x=T, all.y=F, sort=F)

          # reorder rows after merge
          temp <- temp[order(temp$n), ]

          # reorder columns and drop dx and n
          temp <- temp[ , c("severity","issbr")]

          if(calc_method == 2){
              # replace severity=6 with severity=5
              temp[which(temp$severity==6), "severity"] <- 5
          }

          #rename columns
          names(temp) <- paste0(c("sev_","issbr_"), i)

          # add temp columns to dataframe
          message(paste0('inserting columns for ', dx_name))
          df <- .insert_columns(df, dx_name, temp)

      }

      #----------------------------------------------------#
      # Create variables for maximum AIS/ISS body region.  #
      #----------------------------------------------------#
      # i=1
      message("calc max ais for each body region")
      # body regions are coded as text
      body_regions <- unique(i10_map_emp$issbr)
      # make usable for column names
      issbr_names <- gsub("/", "_", body_regions)

      # for each of the 6 body regions loop through dx codes and get max ais for that body region
      for(i in body_regions){
            # Get severity columns and multiply by 1 if they are for body region i and 0 otherwise
            # This uses element-wise multiplication of matricies
            # all severity columns as a matrix * Indicator matrix of body region columns (entries 1 or 0)
            temp <- df[ , grepl("sev_", names(df)), drop=F] * (1*(df[ , grepl("issbr_", names(df))] == i))

            # convert all zeros to NA. zeros represent severity values not associated with body region i
            # temp <- data.frame(t(apply(temp, 1, function(x) ifelse(x==0, NA, x))))

            # take max (excluding 9) and assign to mxaisbr_i
            # severity score of 9 implies unknown severity.
            # Thus we want to exclude these as long as there is at least one known severity for the body region
            # However if all severity scores for the body region are 9 then we will assign maxaisbr a value of 9
            df[ , paste0("mxaisbr_", gsub("/","",i))] <- apply(temp, 1, function(row){

                  # convert all zeros to NA. zeros represent severity values not associated with body region i
                  row <- ifelse(row==0, NA, row)

                  if(all(is.na(row))){
                        maxaisbr <- 0
                  } else if(all(row == 9, na.rm = T)){
                        maxaisbr <- 9
                  } else {
                        maxaisbr <- max(c(0, row[row != 9]), na.rm = T)
                  }
                  return(maxaisbr)
            })
      }

      #----------------------------------------------------------------------#
      #  Calculate maximum severity over all ISS body regions. excluding 9s  #
      #----------------------------------------------------------------------#

      # define function to convert 9 to 0
      c9to0 <- function(x) ifelse(x==9, 0, x)

      # df$maxais_ex9 <- pmax(c9to0(df$mxaisbr1),
      #                   c9to0(df$mxaisbr2),
      #                   c9to0(df$mxaisbr3),
      #                   c9to0(df$mxaisbr4),
      #                   c9to0(df$mxaisbr5),
      #                   c9to0(df$mxaisbr6))

      # assign maxais
      # this is a bit complicated
      # if all maxbr_i are in (9,0,NA) then the max should be 9
      # if there is at least one positive maxbr_i that is not 9 then we need to exclude the 9s
      # for each row in df...
      df$maxais <- apply(df, 1, function(row){
            # select mxaisbr columns
            row <- row[grepl("mxaisbr", names(row))]
            # if the max excluding 9 is zero then include 9 so that if there is a 9 then the max will be 9
            if(all(is.na(row))){
                  maxais <- as.numeric(NA)
            } else if(max(c9to0(row), na.rm = T) == 0){
                  maxais <- max(row, na.rm = T)
            } else {
                 maxais <- max(c9to0(row), na.rm = T)
            }
            return(maxais)
      })
      df$maxais <- as.numeric(df$maxais)

      #------------------------#
      #  Calculate ISS value.  #
      #------------------------#

      # ISS is calculated as the sum of the squared three highest maxaisbr varaiables for a given person.
      # We need to exclude 9s in this calculation.
      df$riss <- apply(df, 1, function(row){
            # select the max ais variables for a given row
            temp <- row[grepl("^mxaisbr", names(row))]

            # for some reason apply is converting these to char. We will convert them back to numeric
            # also convert 9s to 0
            temp <- as.numeric(c9to0(temp))

            # Take the three highest, square them, and sum the result
            # print(temp[order(-temp)[1:3]])
            sum(temp[order(-temp)[1:3]]^2)
            # what if there are only two?
            # In that case this code will sum the squares of the highest two
            # Is this what we want?
      })

      # Replace ISS value with 75 if maximum severity is 6. This implies that the person is dead.
      # 75 is the max ISS score 3(5^2) = 75
      df[df$maxais == 6,"riss"] <- 75

      # Replace ISS value with NA if maximum severity is 9.
      # If maxais is 9 then it implies that there were only injuries of unknown severity
      df[df$maxais == 9, "riss"] <- NA


      #---------------------------------------------------------------------#
      #  Merge diagnosis codes with E-Code reference table to obtain major  #
      #  mechanism, minor mechanism and intent variables for up to 4        #
      #  E-Codes and add them to the data.                                  #
      #---------------------------------------------------------------------#

      # get ecode column names
      ecode_colnames <- paste0("ecode_", 1:4)

      #create ecode columns
      df[ , ecode_colnames] <- NA

      # for each row extract the first 4 ecodes and add them to the e-code columns
      # icd10 e-codes do not start with E.

      # get a list of all ecodes (includes icd10 code if requested)
      ecode_regex <- paste0("^", etab$dx, collapse = "|")

      df[ , ecode_colnames] <- t(apply(df, 1, function(row){
            # remove decimal
            row <- sub("\\.", "", row)
            # get all e codes using pattern matching
            row_ecodes <- stringr::str_extract(as.character(unlist(row)), ecode_regex)
            # remove na values
            row_ecodes <- na.omit(row_ecodes)
            # save first 4 Ecodes
            row_ecodes[1:4]
      }))

      # loop through e codes and add associated variables from e code table
      for(i in 1:4){
          col_name <- paste("ecode_", i, sep="")

          # subset dataframe: pull just the diagnosis code column of interest
          df_ss <- df[,col_name, drop=F]

          # add row variable for sorting back to original order
          df_ss$n <- 1:NROW(df_ss)

          # strip out decimal
          df_ss[,col_name] <- sub("\\.","", df_ss[,col_name])

          # merge in ecode variables
          temp <- merge(df_ss, etab, by.x=col_name, by.y="dx", all.x=T, all.y=F, sort=F)

          # reorder rows after merge
          temp <- temp[order(temp$n), ]

          #  drop dx and n
          temp <- temp[,c("mechmaj", "mechmin", "intent")]

          # rename columns
          names(temp) <- paste(c("mechmaj", "mechmin", "intent"), i, sep="")

          # add columns to dataframe
          df <- .insert_columns(df, col_name, temp)

      }

      # set rownames
      rownames(df) <- 1:nrow(df)

      # return dataframe
      df
}


