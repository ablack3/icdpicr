# Revised 10/15/2010 #
# Version 3.0 #



#-------------------------------------------------------------------------------------------------------------------#
#  Program to add to the user's data, variables indicating the presence of Elixhauser comorbidities and the number  #
#  of Elixhauser comorbidities present.  The user's data should contain a numerical variable with Diagnosis         #
#  Related Group (DRG) information.                                                                                 #
#-------------------------------------------------------------------------------------------------------------------#

#-------------------------------------------------------------------------------------------------------------------#
#  This program is part of larger group of programs collectively known as ICDPIC.  Version 3.0 programs may be      #
#  downloaded from the SSC archives website or installed from within STATA using the ssc command.  Version 3.0      #
#  requires STATA 8.0 or higher.                                                                                    #
#-------------------------------------------------------------------------------------------------------------------#

#-------------------------------------------------------------------------------------------------------------------#
#  NOTE:  This program is a STATA interpretation of SAS software found on the Healthcare Cost and Utilization       #
#  Project (HCUP) website:  https://www.hcup-us.ahrq.gov/toolssoftware/comorbidity/comorbidity.jsp#download.  It     #
#  uses data on comorbidity categories, comorbidity diagnosis codes and DRG codes found on their website.  This     #
#  data is valid through September 30, 2008.  This data may differ from that found in Elixhauser's original         #
#  article.  Notably, Elixhauser's second comorbidity category of Cardiac Arrhythmias has been dropped.  Congest-   #
#  ive Heart Failure, Complicated Hypertension and Renal Failure comorbidity categories no longer have mutually     #
#  exclusive diagnosis codes.  Numerous diagnosis codes and DRG codes have been added to comorbidity categories,    #
#  while a few others have been dropped.  This program is only valid for use with DRG's prior to Version 25.        #
#-------------------------------------------------------------------------------------------------------------------#

#-------------------------------------------------------------------------------------------------------------------#
#  NOTE: Variables representing Elixhauser's comorbidity categories are numbered in order, elix1-elix30.  The       #
#  user will notice the absence of variable elix2, which represents the Cardiac Arrhythmias category which is not   #
#  included in this program.  See note above.                                                                       #
#-------------------------------------------------------------------------------------------------------------------#

#-------------------------------------------------------------------------------------------------------------------#
#  NOTE: Variables elix6A and elix6B represent the existence of hypertension, uncomplicated and complicated         #
#  respectively, before combining into the hypertension, combined, category elix6.                                  #
#-------------------------------------------------------------------------------------------------------------------#


elixhaus <- function(df, dx_pre, drg){

      # Check if user entered a correct name for the Diagnosis Related Group variable. #
        stopifnot(drg %in% names(df))

      # If the data contains no observations. #
        stopifnot(nobs(df)>0)

      # Determine how many diagnosis code variables there are in the data. #
        numdx <- count_pre(df,dx_pre)
        stopifnot(numdx>0)

      # add variable for sorting on at the end
        df$.rec_no <- 1:nobs(df)

      # Create Elixhauser comorbidity variables and number of Elixhasuer comorbidities present variable. #

      df2 <- data.frame(df, elix1=0, elix3=0, elix4=0, elix5=0, elix6=0, elix6A=0, elix6B=0, elix7=0, elix8=0, elix9=0, elix10=0, elix11=0, elix12=0, elix13=0, elix14=0,
       elix15=0, elix16=0, elix17=0, elix18=0, elix19=0, elix20=0, elix21=0, elix22=0, elix23=0, elix24=0, elix25=0, elix26=0, elix27=0, elix28=0, elix29=0,
       elix30=0, elix_cnt=0)

      # Create temporary variable, temp_6B, to hold temporary values for the elix6B variable. #
      df2$.temp_6B = 99

      # Merge with diagnosis related group table. #
        df3 <- merge(df2, drgtab_h, by.x=drg, by.y="drgroup", all.x=T, all.y=F, sort=F )


      # While index number value is less than or equal to maximum number of diagnosis codes. #
      for(i in 1:num_dx) {

            # Generate name of current diagnosis code variable. #
            curr_dx <- paste(dx_pre, i, sep="")

            # strip out . and -
            df3$.dx_temp <- gsub("[.]|[-]","",df3[,curr_dx])

            # Merge with Elixhauser diagnosis code table. #
            df4 <- merge(df3, xtab_s1h, by.x=".dx_temp", by.y="dx", all.x=T, all.y=F, sort=F )


            # Delete the _merge variable created by the merge process. #
            df4 <- subset(df4, select= -.dx_temp)

            # Check for congestive heart failure. #
            df4$elix1 <- with(df4, pmax(exlix1, 1*(elixhaus == "1" | elixhaus == "6B2" | elixhaus == "6B6" | elixhaus == "6B8") ))

            # Check for valvular disease. #
             df4$exlix3 <- pmax(df4$elix3, 1*(df4$elixhaus == "3"))

            # Check for pulmonary circulation disorders. #
             df4$elix4 <- pmax(df$elix4, 1*(df4$elixhaus == "4"))

            # Check for peripheral vascular disorders. #
             df4$elix5 <- pmax(df4$elix5, 1*(df4$elixhaus == "5"))

            # Check for hypertension, uncomplicated. #
             df4$elix6A <- pmax(df4$elix5, 1*(df4$elixhaus == "6A"))

            # Check for hypertension, complicated. #
             df4$elix6B <- pmax(df4$exlix6B, 1*(substr(df4$elixhaus, 1, 2) == "6B"))

            # Generate temporary values for hypertension, complicated. #
             #df4$.temp_6B = real(substr(elixhaus, 3, 1)) if substr(elixhaus, 1, 2) == "6B"
             df4[which(substr(df4$elixhaus, 1, 2) == "6B"),".temp_6B"] <- as.numeric(substr(df4$elixhaus, 3, 4))

            # Check for paralysis. #
             df4$elix7 <- pmax(df4$elix7, 1*(df4$elixhaus == "7"))

            # Check for other neurological disorders. #
             df4$elix8 <- pmax(df4$elix8, 1*(df4$elixhaus == "8"))

            # Check for chronic pulmonary disease. #
             df4$elix9 <- pmax(df4$elix9, 1*(df4$elixhaus == "9"))

            # Check for diabetes, uncomplicated. #
             df4$elix10 <- pmax(df4$elix10, 1*(df4$elixhaus == "10"))

            # Check for diabetes, complicated. #
             df4$elix11 <- pmax(df4$elix11, 1*(df4$elixhaus == "11"))

            # Check for hypothyroidism. #
             df4$elix12 <- pmax(df4$elix12, 1*(df4$elixhaus == "12"))

            # Check for renal failure. #
             df4$elix13 <- pmax(df4$elix13, 1*(df4$elixhaus %in% c("13","6B4","6B7","6B8")))

            # Check for liver disease. #
             df4$elix14 <- pmax(df4$elix14, 1*(df4$elixhaus == "14"))

            # Check for peptic ulcer disease, excluding bleeding. #
             df4$elix15 <- pmax(df4$elix15, 1*(df4$elixhaus == "15"))

            # Check for AIDS. #
             df4$elix16 <- pmax(df4$elix16, 1*(df4$elixhaus == "16"))

            # Check for lymphoma. #
             df4$elix17 <- pmax(df4$elix17, 1*(df4$elixhaus == "17"))

            # Check for metastatic cancer. #
             df4$elix18 <- pmax(df4$elix18, 1*(df4$elixhaus == "18"))

            # Check for solid tumor. #
             df4$elix19 <- pmax(df4$elix19, 1*(df4$elixhaus == "19"))

            # Check for rheumatoid arthritis/collagen vascular diseases. #
             df4$elix20 <- pmax(df4$elix20, 1*(df4$elixhaus == "20"))

            # Check for coagulopathy. #
             df4$elix21 <- pmax(df4$elix21, 1*(df4$elixhaus == "21"))

            # Check for obesity. #
            df4$elix22 <- pmax(df4$elix22, 1*(df4$elixhaus == "22"))

            # Check for weight loss. #
            df4$elix23 <- pmax(df4$elix23, 1*(df4$elixhaus == "23"))

            # Check for fluid and electrolyte disorders. #
            df4$elix24 <- pmax(df4$elix24, 1*(df4$elixhaus == "24"))

            # Check for blood loss anemia. #
            df4$elix25 <- pmax(df4$elix25, 1*(df4$elixhaus == "25"))

            # Check for deficiency anemias. #
            df4$elix26 <- pmax(df4$elix26, 1*(df4$elixhaus == "26"))

            # Check for alcohol abuse. #
            df4$elix27 <- pmax(df4$elix27, 1*(df4$elixhaus == "27"))

            # Check for drug abuse. #
            df4$elix28 <- pmax(df4$elix28, 1*(df4$elixhaus == "28"))

            # Check for psychoses. #
            df4$elix29 <- pmax(df4$elix29, 1*(df4$elixhaus == "29"))

            # Check for depression. #
            df4$elix30 <- pmax(df4$elix30, 1*(df4$elixhaus == "30"))

            # Drop variable elixhaus added by the merge process. #
            df4 <- subset(df4, select=-elixhaus)
      }

      # Update for congestive heart failure with cardiac DRG. #
       #elix1 = 0 if drg_cat == "A"
       df4[which(df4$drg_cat =="A"), "elix1"] <- 0

      # Update for valvular disease with cardiac DRG. #
       #elix3 = 0 if drg_cat == "A"
       df4[which(df4$drg_cat =="A"), "elix3"] <- 0

      # Update for pulmonary circulation disorders with cardiac or COPD or asthma DRG's. #
       #elix4 = 0 if drg_cat == "A" | drg_cat == "B" | drg_cat == "G"
       df4[which(df4$drg_cat == "A" | df4$drg_cat == "B" | df4$drg_cat == "G"), "elix4"] <- 0

      # Update for peripheral vascular disorders with peripheral vascular DRG. #
       #elix5 = 0 if drg_cat == "C"
       df4[which(df4$drg_cat =="C"), "elix5"] <- 0

      # Update for hypertension, uncomplicated, with hypertension DRG. #
       #elix6A = 0 if drg_cat == "D"
       df4[which(df4$drg_cat =="D"), "elix6A"] <- 0



      #-------------------------------------------------------------------------------------------------------#
      #  Begin update for hypertension, complicated, with hypertension or cardiac or renal DRG combinations.  #
      #-------------------------------------------------------------------------------------------------------#
#
#       # Hypertension, complicated, or pre-existing hypertension complicating pregnancy with hypertension, complicated, DRG.                                                                                 #
#        #elix6B = 0 if (temp_6B == . | temp_6B == 0) & (drg_cat == "D" | drg_cat == "DF")
#
#
#       # Hypertensive heart disease, with or without heart failure, with cardiac or hypertension, complicated, DRG's. #
#       # elix6B = 0 if (temp_6B == 1 | temp_6B == 2) & (drg_cat == "A" | drg_cat == "D" | drg_cat == "DF")
#
#       # Hypertensive renal disease, with or without renal failure, with renal failure with kidney transplant or renal failure/dialysis or hypertension, complicated, DRG's.                                             #
#        elix6B = 0 if (temp_6B == 3 | temp_6B == 4) & (drg_cat == "Z" | drg_cat == "KZ" | drg_cat == "LZ" | drg_cat == "QZ" | drg_cat == "D" | drg_cat == "DF")
#
#       # Hypertensive heart and renal disease, without heart or renal failure, or with heart failure only, or
#       # with renal failure only, or with both heart and renal failure, or other hypertension in pregnancy with
#       # cardiac or renal failure with kidney transplant or renal failure/dialysis or hypertension, complicated, DRG's. #
#        elix6B = 0 if (temp_6B == 5 | temp_6B == 6 | temp_6B == 7 | temp_6B == 8 | temp_6B == 9) &
#            (drg_cat == "A" | drg_cat == "Z" | drg_cat == "KZ" | drg_cat == "LZ" | drg_cat == "QZ" | drg_cat == "D" | drg_cat == "DF")
#
#       # Hypertensive renal disease with renal failure, hypertensive heart and renal disease with renal failure or
#       #   hypertensive heart and renal disease, with heart and renal failure.                                       #
#        elix13 = 0 if (temp_6B == 4 | temp_6B == 7 | temp_6B == 8) & (drg_cat == "Z" | drg_cat == "KZ" | drg_cat == "LZ" | drg_cat == "QZ")


      #-----------------------------------------------------------------------------------------------------#
      #  End update for hypertension, complicated, with hypertension or cardiac or renal DRG combinations.  #
      #-----------------------------------------------------------------------------------------------------#


      # Update for paralysis with cerebrovascular DRG. #
       df4[which(df4$drg_cat == "EF"), "elix7"] <- 0

      # Update for other neurological disorders with nervous system DRG. #
       df4[with(df4, which(drg_cat == "DF" | drg_cat == "EF" | drg_cat == "F" | drg_cat == "FQ")), "elix8"] <- 0

      # Update for chronic pulmonary disease with COPD or asthma DRG's. #
       df4[with(df4, which(drg_cat == "B" | drg_cat == "G")), "elix9"] <- 0

      # Update for diabetes, uncomplicated, with diabetes DRG. #
       df4[which(df4$drg_cat == "H"), "elix10"] <- 0

      # Update for diabetes, complicated, with diabetes DRG. #
       df4[which(df4$drg_cat == "H"), "elix11"] <- 0

      # Update for hypothyroidism with thyroid or endocrine DRG's. #
       df4[with(df4, which(drg_cat == "I" | drg_cat == "J")), "elix12"] <- 0

      # Update for renal failure with kidney transplant or renal failure/dialysis DRG's. #
       df4[with(df4, which(drg_cat == "LZ" | drg_cat == "KZ")), "elix13"] <- 0

      # Update for liver disease with liver DRG. #
       df4[with(df4, which(drg_cat == "M" | drg_cat == "MQ")), "elix14"] <- 0

      # Update for peptic ulcer disease, excluding bleeding, with GI hemorrhage or ulcer DRG. #
       df4[which(df4$drg_cat == "N"),"elix15"] <- 0

      # Update for AIDS with HIV DRG. #
       df4[which(df4$drg_cat == "O"), "elix16"] <- 0

      # Update for lymphoma with leukemia/lymphoma DRG. #
       df4[with(df4, which(drg_cat == "P" | drg_cat == "PQ")), "elix17"] <- 0

      # Update for metastatic cancer with cancer DRG. #
       df4[which(df4$drg_cat %in% c("FQ","MQ","PQ","Q","QZ")), "elix18"] <- 0

      # Update for solid tumor with metastasis with cancer DRG. #
       df4[which(drg_cat %in% c("FQ","MQ","PQ","Q","QZ")), "elix19"] <- 0

      # Update for rheumatoid arthritis/collagen vascular diseases with connective tissue DRG. #
       df4[which(df4$drg_cat == "R"), "elix20"] <- 0

      # Update for coagulopathy with coagulation DRG. #
       df4[which(df4$drg_cat == "S"), "elix21"] <- 0

      # Update for obesity with obesity procedure or nutrition/metabolic DRG's. #
       df4[which(df4$drg_cat == "T" | drg_cat == "U"), "elix22"] <- 0

      # Update for weight loss with nutrition/metabolic DRG. #
       df4[which(df4$drg_cat == "U"), "elix23"] <- 0

      # Update for fluid and electrolyte disorders with nutrition/metabolic DRG. #
       df4[which(df4$drg_cat == "U"), "elix24"] <- 0

      # Update for blood loss anemia with anemia DRG. #
       df4[which(df4$drg_cat == "V"), "elix25"] <- 0

      # Update for deficiency anemias with anemia DRG. #
       df4[which(df4$drg_cat == "V"), "elix26"] <- 0

      # Update for alcohol abuse with alcohol or drug DRG. #
       df4[which(df4$drg_cat == "W"), "elix27"] <- 0

      # Update for drug abuse with alcohol or drug DRG. #
       df4[which(df4$drg_cat == "W"), "elix28"] <- 0

      # Update for psychoses with psychoses DRG. #
       df4[which(df4$drg_cat == "X"), "elix29"] <- 0

      # Update for depression with depression DRG. #
       df4[which(df4$drg_cat == "Y"), "elix30"] <- 0

      # Set uncomplicated hypertension to FALSE if complicated hypertension is TRUE. #
       df4[which(df4$elix6B == 1), "elix6A"] <- 0

      # Set uncomplicated diabetes to FALSE if complicated diabetes is TRUE. #
       df4[which(df4$elix11 == 1), "elix10"] <- 0

      # Set solid tumor without metastasis to FALSE if metastatic cancer is TRUE. #
       df4[which(df4$elix18 == 1), "elix19"] <- 0

      # Combine hypertension without complications and hypertension with complications. #
       df4$elix6 <- pmax(df4$elix6, 1*(df4$elix6A == 1 | df4$elix6B == 1))

      # Calculate number of Elixhauser comorbidities present. #
       df4$elix_cnt <- rowSums(df4[,paste("elix",c(1,3:30),sep="")])

      # Sort table on num_rec variable. #
       df4 <- df4[order(df4$.rec_no),]

      # Drop temporary variables rec_no, temp_6B and variable drg_cat. #

      df4 <- subset(df4, select=-c(.rec_no,  .temp_6B,  drg_cat))

      # Save new version of table to disk. #
      df4

}

