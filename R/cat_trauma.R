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
#'
#' @param icd10 Should ICD 10 codes be included? Must be one of: TRUE, FALSE, "cm", or "base".
#'          \itemize{
#'          \item TRUE ICD10CM codes will be processed by the program
#'          \item FALSE - No ICD codes will be considered by cat_trauma(). Any ICD10 codes in the data then they will be ignored.
#'          \item "cm" - ICD10CM codes will be processed by the program
#'          \item "base" - ICD10 (international) codes will be processed by cat_trauma()
#'          }
#'          If the icd10 argument is not set to FALSE then the method used to map ICD 10 codes to AIS is determined by the i10_iss_method argument.
#'
#' @param i10_iss_method Method for calculating ISS from ICD10 codes. Ignored if icd10 = FALSE. Must be one of:
#'          \itemize{
#'          \item "roc_max_NIS" Table derived empirically from National Inpatient Sample (NIS) using ROC c-stat as the objective. For ICD10 codes not in NIS the mapping based on TQIP data will be used as a backup. This option is recommeded if the users data is similar to NIS data. Details of the mapping algorithm included in ICDPIC-R package help documentation.
#'          \item "roc_max_TQIP" Table derived empirically from the Trauma Quality Improvement Program data using ROC c-stat as the objective. For ICD10 codes not in TQIP the mapping based on NIS data will be used as a backup. This option is recommended if the user's data is similar to the TQIP data.
#'          \item "roc_max_NIS_only" Table derived empirically from National Inpatient Sample using ROC c-stat as the objective. Injury ICD10 codes not in the NIS dataset will be ignored.
#'          \item "roc_max_TQIP_only" Table derived empirically from Trauma Quality Improvement Program data using ROC c-stat as the objective. Injury ICD10 codes not in the TQIP dataset will be ignored.
#'          \item "gem_max" Table derived by mapping ICD 10 to ICD 9 using the CMS general equivalence mapping tables and then to ISS
#'                 using the original ICDPIC table. Mapping conflicts handled by taking the max ISS.
#'          \item "gem_min" Same as "gem_max" except that mapping conflicts are handled by taking the min ISS.
#'          }
#'
#' @param calc_method ISS calculation method:
#'          Method 1 (default) will assign an ISS of 75 if any AIS is 6.
#'          Method 2 will change any AIS = 6 to 5 and then calculate ISS normally.
#'
#' @param verbose Should updates be printed to the console? TRUE or FALSE (default). This can be helpful for long running computations.
#'
#' @return A dataframe identical to the dataframe passed to the function with the following additional variables
#'          added:
#'          \itemize{
#'          \item sev_1-sev_n: AIS severity for diagnosis codes 1..n
#'          \item issbr_1-issbr_n: ISS body region for diagnosis codes 1..n
#'          \item mxaisbr1-mxaisbr6: maximum AIS severity for each of the 6 ISS body regions
#'          \item maxais: maximum AIS severity over all ISS body regions
#'          \item riss: computed injury severity score
#'          \item niss: new injury severity score
#'          \item ecode_1-ecode_4: first 4 mechanism/E-Codes (including ICD10 if requested) found in each row of data
#'          \item mechmaj1-mechmaj4: CDC external cause of injury major mechanism for each E-Code captured
#'          \item mechmin1-mechmin4: CDC external cause of injury minor mechanism for each E-Code captured
#'          \item intent1-intent4: intent for each E-Code captured
#'          \item lowmech: lowest CDC external cause of injury major mechanism for all E-Codes captured
#'          \item mortality_prediction: The model predicted probability of mortaility. (only added if using ICD 10 codes with roc_max method)
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
#' @examples df_in <- read.table(header = TRUE, text = "
#' ident    dx1     dx2     dx3
#' 31416   800.1   959.9   E910.9
#' 31417   800.24  410.0   NA
#' ")
#' df_out <- cat_trauma(df_in, "dx")
#'
#' @importFrom stringr str_extract
#' @importFrom stats na.omit
#' @export
cat_trauma <- function(df, dx_pre, icd10, i10_iss_method, calc_method = 1, verbose = F){

      # Verify input #
      if(!is.data.frame(df)) stop("First argument must be a dataframe")
      if(NROW(df) == 0) stop("Data contains no observations. It must contain at least one row.")
      if(!is.character(dx_pre)) stop("Second argument must be a character string")
      # ensure dx_pre is a valid variable name
      if(make.names(dx_pre) != dx_pre) stop("Second argument must be a valid variable name in R")
      if(!(calc_method %in% c(1,2))) stop("calc_method must be either 1 or 2")
      if(!(icd10 %in% c(T, F, "cm", "base"))) stop("icd10 must be TRUE, FALSE, 'cm', or 'base'")
      if(i10_iss_method == "roc_max") stop("The roc_max option has been depricated. Please use roc_max_NIS, roc_max_TQIP, roc_max_NIS_only, or roc_max_TQIP_only instead.")
      if((icd10 != F) && !(i10_iss_method %in% c("roc_max_NIS", "roc_max_TQIP", "roc_max_NIS_only", "roc_max_TQIP_only" ,"gem_max", "gem_min"))) stop("i10_iss_menthod must be roc_max_NIS, roc_max_TQIP, roc_max_NIS_only, roc_max_TQIP_only, gem_max, or gem_min.")

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

      # Treat icd==T the same as icd=="cm"
      if(isTRUE(icd10)) icd10 <- "cm"

      # If ICD10 codes are requested then add ICD10 codes to the lookup tables
      # The i10 mappings  for n codes were created by using both CMS general equivalence mappings
      # and Dave's empirical method
      # The i10 mapings for e codes (mechanism) were created using CDC injury mechanism grid
      # See documentation and prelim directory for details
      if(icd10 %in% c("base", "cm")){
            etab <- rbind(etab_s1, i10_ecode)

            ntab <- switch(i10_iss_method,
                           roc_max_NIS = rbind(ntab_s1, .select_i10_data("NIS", icd10)),
                           roc_max_TQIP = rbind(ntab_s1, .select_i10_data("TQIP", icd10)),
                           roc_max_NIS_only = rbind(ntab_s1, .select_i10_data("NIS_only", icd10)),
                           roc_max_TQIP_only = rbind(ntab_s1, .select_i10_data("TQIP_only", icd10)),
                           gem_max = rbind(ntab_s1, i10_map_max),
                           gem_min = rbind(ntab_s1, i10_map_min))
      } else {
            ntab <- ntab_s1
            etab <- etab_s1
      }

      #---------------------------------------------------------------------------------#
      #  Merge diagnosis code variables with N-Code reference table to obtain severity  #
      #  and ISS body region variables for each diagnosis code and add them to the data #
      #---------------------------------------------------------------------------------#
      # message("inserting severity and body_region columns")
      for(i in dx_nums){
          # create column name
          dx_name <- paste0(dx_pre, i)

          # pull just the diagnosis code column of interest
          df_ss <- df[ , dx_name, drop=F]

          # add row variable for sorting back to original order
          df_ss$n <- 1:NROW(df_ss)

          # strip out decimal in all codes
          df_ss[ , dx_name] <- sub("\\.", "", df_ss[ , dx_name])

          # For ICD 9 codes we do not need to check the format since only valid codes will be matched
          # when we merge in the ISS from the lookup tables
          # Note that this assumes that no ICD 10 codes will inadvertently be matched to ICD 9 codes
          # I think this is true but am not 100% sure yet.
          # OK...I did some checking and V codes are a problem.
          # V12 is both a valid I9 and I10 code for example
          # E codes are also a problem if we strip the decimal. E800 after decimal stripping
          # is in both I9 and I10 (E80.0).

          # Luckily we are only dealing with a subset of I9 and I10
          # for the N-codes
          # The I9 subset only include codes that start with 8 or 9
          # The I10 subset only includes codes that start with S or T

          # for the E codes (nature of injury codes)
          # I10 start with "U" "V" "W" "X" "Y"
          # I9 start with "E"

          # Is it possible that an I9 V code gets classified as I10?
          # Yep seems so. V20 will be matched with I10 even though it could be an I9 code.

          # The same is not true of the ICD 10 codes since there are placeholder "X" characters
          # allowed. If the user requests ICD 10 then we need to validate ICD 10 codes and
          # the process them according to what Dave did when he created the empirical table.

          # The National Trauma Data Standard used by NTDB considers valid ICD-10-CM injury
          # codes to be those in the ranges S00-S99, T07, T14, T20-T28, and T30-32.
          # ICDPIC-R recognizes only these codes in the calculation of injury severity from ICD-10
          # and also requires that the codes have a decimal point in the fourth position and the
          # letter 'A' in the eighth position (indicating an initial encounter).
          # We only need to do this processing for the roc_max method.
          # If user is using the GEM then the code validation is automatically handled
          # through the merge just like in the icd9 case.

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
                  if(!substr(s,1,1) %in% c("S","T","U","V","W","X","Y")) {
                     ret_val <- s
                  } else if(nchar(s) < 7 & !grepl("X", substr(s, 2, nchar(s)))) {
                     ret_val <- s
                  } else if(nchar(s) != 7) {
                     ret_val <- ""
                  } else if(substr(s,7,7) != "A") {
                     ret_val <- ""
                  } else if(substr(s,5,5) == "X") {
                     ret_val <- substr(s,1,4)
                  } else if(substr(s,6,6) == "X") {
                     ret_val <- substr(s,1,5)
                  } else {
                     ret_val <- substr(s,1,6)
                  }
                  return(ret_val)
              }

              # process the codes
              df_ss[ , dx_name] <- sapply(df_ss[ , dx_name], process_i10)
              # tst$dx12 <- sapply(tst$dx1, process_i10)
              # process_i10("S80.812A")
              # process_i10("S0189")
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
          # message(paste0('inserting columns for ', dx_name))
          df <- .insert_columns(df, dx_name, temp)

      }

      #----------------------------------------------------#
      # Create variables for maximum AIS/ISS body region.  #
      #----------------------------------------------------#
      # i=1
      # message("calc max ais for each body region")
      # body regions are coded as text
      body_regions <- unique(i10_map_max$issbr)
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
            # what if there are only two?
            # In that case this code will sum the squares of the highest two
            # Is this what we want?
            sum(temp[order(-temp)[1:3]]^2)
      })

      # Replace ISS value with 75 if maximum severity is 6. This implies that the person is dead.
      # 75 is the max ISS score 3(5^2) = 75
      df[df$maxais == 6,"riss"] <- 75

      # Replace ISS value with NA if maximum severity is 9.
      # If maxais is 9 then it implies that there were only injuries of unknown severity
      df[df$maxais == 9, "riss"] <- NA

      #------------------------------------------------#
      # Calculate the New Injury Severity Score (NISS) #
      #------------------------------------------------#

      # ISS is calculated as the sum of the squared three highest ais varaiables for a given person.
      # We need to exclude 9s in this calculation.
      df$niss <- apply(df, 1, function(row){
         # select the max ais variables for a given row
         temp <- row[grepl("^sev_", names(row))]
         # convert NA to 0
         temp <- as.numeric(temp)
         temp <- ifelse(is.na(temp) | temp == 9, 0, temp)
         # Take the three highest, square them, and sum the result
         sum(temp[order(-temp)[1:3]]^2)
      })

      # Replace ISS value with 75 if maximum severity is 6. This implies that the person is dead.
      # 75 is the max ISS score 3(5^2) = 75
      df[df$maxais == 6,"niss"] <- 75

      # Replace ISS value with NA if maximum severity is 9.
      # If maxais is 9 then it implies that there were only injuries of unknown severity
      df[df$maxais == 9, "niss"] <- NA


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

      #---------------------------------------------#
      # Add mortality prediction if possible        #
      #---------------------------------------------#
      if(stringr::str_detect(i10_iss_method, "NIS|TQIP") && icd10 %in% c("cm", "base")) {
            if(verbose) print("Calculating mortality prediction")

            coef_df <- .select_i10_coef(prefix = stringr::str_extract(i10_iss_method, "NIS|TQIP"), icd10)
            stopifnot(max(coef_df$intercept, na.rm = T) == min(coef_df$intercept, na.rm = T))
            intercept <- max(coef_df$intercept, na.rm = T)

            # create hash table
            coef_df <- coef_df[!is.na(coef_df$effect), ]
            effect_hash <- coef_df$effect
            names(effect_hash) <- coef_df$dx
            calc_mortality_prediction <- function(dx){
               # dx is a character vector of diagnosis codes for one person
               x <- sum(effect_hash[sub("\\.", "", dx)], na.rm = T) + intercept
               1/(1+exp(-x))
            }

            mat <- as.matrix(df[,grepl(paste0("^", dx_pre), names(df))])
            df$mortality_prediction <- apply(mat, 1, calc_mortality_prediction)

      }

      # set rownames
      rownames(df) <- 1:nrow(df)

      # return dataframe
      df
}

