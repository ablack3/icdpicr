#' Categorize trauma by adding AIS and ISS to a dataframe
#'
#' For each observation this function will
#' \enumerate{
#'    \item assign a severity (AIS) and ISS body region values to each valid ICD-9-CM or ICD 10 trauma code,
#'    \item add variables for maximum severity of each body region,
#'    \item calculate ISS and new ISS
#'    \item select first 4 e-codes/mechanism codes along with major mechanism, minor mechanism, and intent
#'    \item summarize mechanism with lowest major mechanism and assign trauma type (blunt or penetrating) the first E-Code found.
#'}
#'
#'
#'
#'
#' @param df A dataframe in wide format containing ICD-9 or ICD-10 diagnosis codes with a common column name prefix.
#'          Diagnosis codes should be character strings and may have a decimal or not.
#' @param dx_pre Prefix for diagnosis code column names (example: dx1, dx2, ect)
#' @param calc_method ISS calculation method:
#'          Method 1 will assign an ISS of 75 if any AIS is 6 assuming the person is dead.
#'          Method 2 will change any AIS = 6 to 5 and then calculate ISS normally.
#' @param conflict_resolution Method for resolving ISS score conflicts when mapping ICD-10 codes to ICD 9
#'          codes to AIS.
#'          Must be either "max" or "min". Few ICD-10 codes result in a conflict.
#' @param icd10 A logical value indicating whether ICD-10 codes should be considered or ignored.
#'          If FALSE then ICD-10 codes are ignored.
#'          If TRUE then ICD-10 codes are handled based on the i10_iss_method argument
#'          and ICD10 mechanism codes will be included in E-code calculation.
#' @param i10_iss_method Method for calculating ISS from ICD10-CM codes. Must be one of:
#'          \itemize{
#'          \item "empirical" Table derived empirically from _____ described in detail in ____ (default)
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
#'          \item bluntpen: type of trauma, blunt (B) or penetrating (P), based on value of variable mechmaj1
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
#' df_out <- trauma(df_in, "dx")
#'
#' @export



#-----------------------------------------------------------------------------------------------------------------#
#  For each observation, program to assign severity and ISS body region values to each valid ICD-9-CM trauma      #
#  code, assign Barell and AP component categories to each valid ICD-9-CM trauma code, calculate injury severity  #
#  score (ISS), assign major mechanism, minor mechanism and intent for up                                         #
#  to 4 E-Codes (excluding E-Code place) and assign trauma type (blunt or penetrating) based on major mechanism   #
#  of the first E-Code found.                                                                                     #
#-----------------------------------------------------------------------------------------------------------------#

# testing
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

cat_trauma <- function(df, dx_pre, calc_method = 1, icd10 = TRUE, i10_iss_method = "empirical"){

      # Verify input #
      if(!is.data.frame(df)) stop("First argument must be a dataframe")
      if(NROW(df) == 0) stop("Data contains no observations.")
      if(!is.character(dx_pre)) stop("Second argument must be a character string")
      # ensure dx_pre is a valid variable name
      if(make.names(dx_pre) != dx_pre) stop("Second argument must be a valid variable name in R")
      if(!(calc_method %in% c(1,2))) stop("calc_method must be either 1 or 2")
      if(!(icd10 %in% c(T, F))) stop("icd10 must be TRUE or FALSE")
      if(!(i10_iss_method %in% c("empirical","gem_max","gem_min"))) stop("i10_iss_menthod must be empirical, gem_max, or gem_min")

      # Check if user entered a correct prefix for the diagnosis code variables in the input file
      # Determine how many diagnosis code variables there are in the data
      regex_dx <- paste0("^", dx_pre, "([0-9]+)$")
      dx_colnames <- grep(regex_dx, names(df), value = T)
      # replace full column name with first capture group and convert to number
      dx_nums <- as.numeric(sub(regex_dx, "\\1", dx_colnames))
      num_dx <- length(dx_nums)
      if(num_dx == 0) stop("No variables with prefix found in data")

      # make sure df is not a tibble and if it is convert back to dataframe
      df <- data.frame(df)

      # add i10 codes to lookup tables
      # The i10 mappings  for n codes were created by using CMS general equivalence mappings
      # The i10 maapings for e codes (mechanism) were created using CDC injury mechanism grid
      # See documentation and prelim directory for details
      if(icd10){
            etab <- rbind(etab_s1, i10_ecode)

            if(i10_iss_method == "empirical"){
                  ntab <- rbind(ntab_s1, i10_map_emp)
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
      print("inserting sev and br columns")
      for(i in dx_nums){
          # create column name
          dx_name <- paste0(dx_pre, i)

          # pull just the diagnosis code column of interest
          df_ss <- df[ , dx_name, drop=F]

          # add row variable for sorting back to original order
          df_ss$n <- 1:NROW(df_ss)

          # strip out decimal and take only one or zero letters followed by numbers
          df_ss[ , dx_name] <- stringr::str_extract(sub("\\.", "", df_ss[ , dx_name]), "^[A-Z]?[0-9]+")

          # merge iss variables
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
          print(paste('inserting columns for ', dx_name))
          df <- .insert_columns(df, dx_name, temp)

      }

      #----------------------------------------------------#
      # Create variables for maximum AIS/ISS body region.  #
      #----------------------------------------------------#
      # i=1
      print("calc max ais for each body region")
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
            # this code will sum the squares of the highest two
            # is this what we want?
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

      # get column names
      dx_colnames <- paste0(dx_pre, 1:num_dx)
      ecode_colnames <- paste0("ecode_", 1:4)

      #create ecode columns
      df[ , ecode_colnames] <- NA

      # for each row extract the first 4 ecodes and add them to the e-code columns
      # icd10 e-codes do not start with E.

      # get a list of all ecodes (includes icd10 code if requested)
      ecodes <- etab$dx

      df[ , ecode_colnames] <- t(apply(df, 1, function(row){
            # remove trailing letters
            row <- sub("[A-Za-z]+$", "", row)

            # remove trailing decimal
            row <- sub("\\.$", "", row)

            # get all e codes, need to strip decimal
            ecode_indicators <- as.character(unlist(sub("\\.","", row))) %in% ecodes
            row_ecodes <- row[ecode_indicators]

          # (E_codes <- grep("^E", row[dx_colnames], value = T))
          # take first 4 e codes that are not place e codes (start with E849)
            row_ecodes[1:4]
            # E_codes[!grepl("^E849", E_codes)][1:4]
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

          #rename columns
          names(temp) <- paste(c("mechmaj", "mechmin", "intent"), i, sep="")

          # add columns to dataframe
          df <- .insert_columns(df, col_name, temp)

      }

      # Create bluntpen variable to hold type of trauma: blunt or penetrating. #
      df$bluntpen = NA

      # Determine blunt or penetrating trauma. #
      df[which(df$mechmaj1 %in% c(2, 5, 6, 7, 8, 9, 13)), "bluntpen"] <- "B"
      df[which(df$mechmaj1 %in% c(0, 4)), "bluntpen"] <- "P"

      # Why are we only considering the first ecode mechmaj here? convenience.
      # It is an arbitrary decision.
      # However it is unlikely that there would be more than 1 mechanism of injury

      # set rownames
      rownames(df) <- 1:nrow(df)

      # return dataframe
      df
}


