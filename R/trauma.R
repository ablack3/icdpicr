#' Add AIS and ISS to a dataframe.
#'
#' For each observation this function will assign a severity and ISS body region values to each valid ICD-9-CM
#' or ICD 10 trauma code,
#' and AP component categories to each valid ICD-9-CM trauma code, calculate injury severity score (ISS) and new
#' injury severity score (NISS), assign major mechanism, minor mechanism and intent for up to 4 E-Codes
#' (excluding E-Code place) and assign trauma type (blunt or penetrating) based on major mechanism  of the first
#' E-Code found.
#'
#' @param df dataframe containing ICD-9 or ICD-10 diagnosis codes with a common prefix
#' @param dx_pre prefix for diagnosis codes (example: dx1, dx2, ect)
#' @param calc_method ISS calculation method: method 1 will assign an ISS of 75 if any AIS is 6
#'          (implying the person is dead?)
#'          method 2 will change any AIS = 6 to 5 and then calculate ISS normally.
#' @param conflict_resolution Method for resolving ISS score conflicts when mapping ICD-10 codes to ICD 9 codes. Must be either "max" or "min".
#' @param icd10 A logical value indicating whether ICD 10 codes should be mapped to ICD 9 using CMS's general equivalence mapping and then included in the calcuation of the ISS or not. Must be TRUE or FALSE.
#'
#' @return A dataframe identical to the dataframe passed to the function with the additional variables added.
#' These variables are severity scores and body regions for each diagnosis code. XISS, NISS...
#'
#' @export



#-----------------------------------------------------------------------------------------------------------------#
#  For each observation, program to assign severity and ISS body region values to each valid ICD-9-CM trauma      #
#  code, assign Barell and AP component categories to each valid ICD-9-CM trauma code, calculate injury severity  #
#  score (ISS) and new injury severity score (NISS), assign major mechanism, minor mechanism and intent for up    #
#  to 4 E-Codes (excluding E-Code place) and assign trauma type (blunt or penetrating) based on major mechanism   #
#  of the first E-Code found.                                                                                     #
#-----------------------------------------------------------------------------------------------------------------#

trauma <- function(df, dx_pre, calc_method = 1, icd10 = TRUE, conflict_resolution="max"){

      # Verify input #
      if(!is.data.frame(df)) stop("First argument must be a dataframe")
      if(!is.character(dx_pre)) stop("Second argument must be a character string")
      #also need to ensure dx_pre is a valid variable name too
      if(!(calc_method %in% c(1,2))) stop("calc_method must be either 1 or 2")
      if(NROW(df) == 0) stop("Data contains no observations.")
      if(!(conflict_resolution %in% c("max","min"))) stop("conflict resolution method must be either max or min")


      # Check if user entered a correct prefix for the diagnosis code variables in the input file.
      # and Determine how many diagnosis code variables there are in the data. #
      regex_dx <- paste(dx_pre,"([0-9]+)",sep="")
      dx_colnames <- grep(regex_dx, names(df), value = T)
      dx_nums <- as.numeric(sub(regex_dx,"\\1",dx_colnames ))
      num_dx <- length(dx_nums)
      if(num_dx == 0) stop("No variables with prefix found in data")


      # add i10 codes to map
      # The i10 mappings were created by using CMS general equivalence mappings to first map to icd9
      if(icd10){
            ntab_s1 <- ntab_s1[ , c("dx", "severity", "issbr")]

            if(conflict_resolution == "max"){
                  names(i10_map_max) <- c("dx", "severity", "issbr")
                  ntab_s1 <- rbind(ntab_s1, i10_map_max)
            }
            else{
                  names(i10_map_min) <- c("dx", "severity", "issbr")
                  ntab_s1 <- rbind(ntab_s1, i10_map_min)

            }
      }


      # Create temporary variable to count the number of diagnosis codes with both an
      # unknown severity and an unknown  ISS body region.
      df$unk_unk <- 0

      #---------------------------------------------------------------------------------#
      #  Merge diagnosis code variables with N-Code reference table to obtain severity  #
      #  and ISS body region variables for each diagnosis code and add them to the data #
      #---------------------------------------------------------------------------------#

      for(i in dx_nums){
          # create column name
          dx_name <- paste(dx_pre, i, sep="")

          # pull just the diagnosis code column of interest
          df_ss <- df[ , dx_name, drop=F]

          # add row variable for sorting back to original order
          df_ss$n <- 1:NROW(df_ss)

          # strip out decimal
          #df_ss[,dx_name] <- sub("\\.","", as.data.frame(df_ss[,dx_name]))
          df_ss[ , dx_name] <- apply(df_ss[ , dx_name, drop=F], 1, function(x) sub("\\.", "", x))

          # merge iss variables
          temp <- merge(df_ss, ntab_s1, by.x=dx_name, by.y="dx", all.x=T, all.y=F, sort=F)

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
          df <- .insert_columns(df, dx_name, temp)

      }

      # Increment value in the temporary variable unk_unk if severity equals 9
      # and ISS body region equals 9.??? #
      #  replace unk_unk = unk_unk + 1 if `sev' == 9 & `issbr' == 9
      for(i in dx_nums){
            df$unk_unk <- df$unk_unk + (df[ , paste0("sev_", i)] == 9 & df[ , paste0("issbr_", i)] == 9)
      }

      # Increment value in the temporary variable unk_unk if severity equals 9
      # and ISS body region equals 9.??? #
      #  replace unk_unk = unk_unk + 1 if `sev' == 9 & `issbr' == 9
      for(i in dx_nums){
            df$unk_unk <- df$unk_unk + (df[ , paste0("sev_", i)] == 9 & df[ , paste0("issbr_", i)] == 9)
      }

      #----------------------------------------------------#
      # Create variables for maximum AIS/ISS body region.  #
      #----------------------------------------------------#
# i=1
      # for each body region 1-6 loop through dx codes and get max ais for that body region
      for(i in 1:6){
            # Get severity columns and multiply by 1 if they are for body region i and 0 otherwise
            # This uses element-wise multiplication of matricies
            temp <- df[ , grepl("sev_", names(df)), drop=F] * (1*(df[ , grepl("issbr_", names(df))] == i))

            class(temp)
            # convert all zeros to NA. zeros represent severity values not associated with body region i
            # temp <- data.frame(t(apply(temp, 1, function(x) ifelse(x==0, NA, x))))

            # take max (excluding 9) and assign to mxaisbr_i
            # severity score of 9 implies unknown severity.
            # Thus we want to exclude these as long as there is at least one known severity for the body region
            # However if all severity scores for the body region are 9 then we will assign maxaisbr a value of 9
            # (row=temp[1,,drop=F])
            # class(row)
            df[ , paste0("mxaisbr", i)] <- apply(temp, 1, function(row){

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
      df$maxais <- apply(df, 1, function(row){
            row <- row[grepl("mxaisbr", names(row))]
            # print(row)
            # if the max excluding 9 is zero then include 9 so that if there is a 9 the max will be 9
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

      # there is more to do here for differences based on known/unknown severity ect... not sure yet
      # time to figure this out


      #------------------------#
      #  Calculate ISS value.  #
      #------------------------#

      # ISS is calculated as the sum of the squared three highest maxaisbr varaiables for a given person
      # need to exclude 9s
      df$xiss <- apply(df, 1, function(row){

            #recode 9s as 0
            row <- c9to0(row)

             # select the max ais variables for a given row
            temp <- row[grepl("^mxaisbr[0-9]+$", names(row))]

            # for some reason apply is converting these to char. We will convert them back to numeric
            temp <- as.numeric(temp)

            # Take the three highest, square them, and sum the result
            sum(temp[order(-temp)[1:3]]^2) # what if there are only two?
      })

      # Replace ISS value with 75 if maximum severity is 6. Why?
      df[df$maxais == 6,"xiss"] <- 75

      # Replace ISS value with 99 if maximum severity is 9. Why?
      df[df$maxais == 9, "xiss"] <- 99


      #-------------------------------------------#
      #  Calculate NISS value.  #
      #-------------------------------------------#

      # NISS is calculated as the sum of the squared three highest severity varaiables for a given person
      # We need to exclude severity values of 9 in this calculation since 9 represents unknown.
      df$niss <- apply(df, 1, function(row){
            # select severity variables for each row
            temp <- row[grepl("^sev_[0-9]+$", names(row))]

            # for some reason apply is converting these to char. We will convert them back to numeric.
            temp <- as.numeric(temp)

            # exclude severity values of 9 (unknown)
            temp <- temp[temp != 9]

            # Take the three highest, square them, and sum the result
            sum(temp[order(-temp)[1:3]]^2, na.rm = T) # what if there are only two. will probably cause error.
      })

      # Replace NISS value with 75 if maximum severity is 6.
      df[df$maxais == 6, "niss"] <- 75

      # Replace ISS value with 99 if maximum severity is 9.
      df[df$maxais == 9, "niss"] <- 99

      #---------------------------------------------------------------------#
      #  Merge diagnosis codes with E-Code reference table to obtain major  #
      #  mechanism, minor mechanism and intent variables for up to 4        #
      #  E-Codes and add them to the data.                                  #
      #---------------------------------------------------------------------#

      # get column names
      dx_colnames <- paste0(dx_pre, 1:num_dx)
      ecode_colnames <- paste0("ecode_", 1:4)

      #create e code columns
      df[ , ecode_colnames] <- NA

      # for each row extract the first 4 ecodes and add them to the ecode columns
      df[ , ecode_colnames] <- t(apply(df, 1, function(row){
          # get all e codes
          (E_codes <- grep("^E", row[dx_colnames], value = T))
          # take first 4 e codes that are not place e codes (start with E849)
          E_codes[!grepl("^E849", E_codes)][1:4]
      }))

      # loop through e codes and add associated variables from e code table
      for(i in 1:4){
          col_name <- paste("ecode_",i,sep="")

          # subset dataframe: pull just the diagnosis code column of interest
          df_ss <- df[,col_name, drop=F]

          # add row variable for sorting back to original order
          df_ss$n <- 1:NROW(df_ss)

          # strip out decimal
          df_ss[,col_name] <- sub("\\.","", df_ss[,col_name])

          # merge in ecode variables
          temp <- merge(df_ss, etab_s1, by.x=col_name, by.y="dx", all.x=T, all.y=F, sort=F)

          # reorder rows after merge
          temp <- temp[order(temp$n),]

          #  drop dx and n
          temp <- temp[,c("mechmaj","mechmin","intent")]

          #rename columns
          names(temp) <- paste(c("mechmaj","mechmin","intent"),i,sep="")

          # add columns to dataframe
          df <- .insert_columns(df,col_name,temp)

      }

      # Create lowmech variable to hold the lowest major E-code mechanism code. #
      df$lowmech <- pmin(df$mechmaj1, df$mechmaj2, df$mechmaj3, df$mechmaj4, na.rm = T)

      # Create bluntpen variable to hold type of trauma: blunt or penetrating. #
      df$bluntpen = NA

      # Determine blunt or penetrating trauma. #
      df[which(df$mechmaj1 %in% c(2, 5, 6, 7, 8, 9, 13)), "bluntpen"] <- "B"
      df[which(df$mechmaj1 %in% c(0, 4)), "bluntpen"] <- "P"

      # #why are we only considering the first ecode mechmaj here? convenience. arbitrary decision.
      # - orignal code below
      # replace bluntpen = "B" if mechmaj1 == 2 | mechmaj1 == 5 | mechmaj1 == 6 | mechmaj1 == 7 | mechmaj1 == 8 |#
      # # mechmaj1 == 9 | mechmaj1 == 13
      # replace bluntpen = "P" if mechmaj1 == 0 | mechmaj1 == 4

      # set rownames
      rownames(df) <- 1:nrow(df)

      #return dataframe
      df
}


