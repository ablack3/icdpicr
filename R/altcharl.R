# Revised 2016 #
# Version 3.0 # 

#--------------------------------------------------------------------------------------------------------------------#
#  Function to add to the user's data variables indicating the presence of Charlson comorbidities, the number of      #
#  Charlson comorbidities present and the Charlson score.                                                            #
#                                                                                                                    #
#  Comorbidity weights are assigned per Charlson^ (1987).  Charlson comorbidities are as modified by Romano# (1993)  #
#  for surgical admissions.  Comorbidity diagnostic categories are included if and only if they include diagnosis    #
#  codes identified by Romano with an asterisk in Table 1 under the column Dartmouth-Manitoba codes.  Within these   #
#  included categories, only diagnosis or procedure codes with asterisks are included.                               #
#                                                                                                                    #
#  For a more standardized treatment of the Charlson index, with options that include ICD-9-CM, ICD-10-CM and        #
#  enhanced ICD-9-CM versions, the user is encouraged to type "findit charlson" from within STATA without the        #
#  quotation marks. STATA will locate this module on the Internet.  Just follow the instructions given to download   #
#  it.                                                                                                               #
#                                                                                                                    #
#  ^Charlson ME, Pompei P, Ales KL, MacKenzie CR.  A new method of classifying prognostic comorbidity in             #
#  longitudinal studies:  Development and validation.  Journal of Chronic Disease 1987; 40:373.                      #
#  #Romano PS, Roos LL, Jollis JG.  Adapting a clinical comorbidity index for use with ICD-9-CM administrative       #
#  data:  Differing perspectives.  Journal of Clinical Epidemiology 1993; 46:1075                                    #
#--------------------------------------------------------------------------------------------------------------------#

#--------------------------------------------------------------------------------------------------------------------#
#  NOTE: Diagnosis and procedure code tables have been updated to reflect new codes within above specified ranges.   #
#  No attempt has been made to identify and include individual codes that may be applicable.                         #
#--------------------------------------------------------------------------------------------------------------------#

altcharl <- function(df, dx_pre, px_pre, switch){
    stopifnot(NROW(df)>0)
    
    dx_cnt <- count_pre(df, dx_pre)
    px_cnt <- count_pre(df, px_pre)
    
    stopifnot(dx_cnt+px_cnt > 0)
    
    # add charlson variables
    df <- data.frame(df, mi=0, pvd=0, dementia=0, copd=0, rheum=0, mld=0, sld=0, mdm=0, ccdm=0,
            renal=0, any_mal=0, mst=0, aids=0, charlcnt=0, charlval = 0)
    
    # add variable for ordering
    df$n <- 1:NROW(df)
    
    
    if(dx_cnt > 0) for(i in 1:dx_cnt){
                
                # Generate name of current diagnosis code variable. #
                cur_dx <- paste(dx_pre, i, sep="")
        
                # Get number of missing observations for the current diagnosis code variable. #
                missing <- sum(is.na(df[,cur_dx]))

                # Merge with Charlson diagnosis code table. #
                if(switch == 1) {
                    temp <-  merge(df[,c(cur_dx,"n")],cdtab_s1, by.x = cur_dx, by.y="dx", all.x=T, sort=F)
                } else if (switch == 2) {
                    temp <-  merge(df[,c(cur_dx,"n")], cdtab_s2, by.x = cur_dx, by.y="dx", all.x=T, sort=F)
                }
                
                #reorder after merge
                temp <- temp[order(temp$n)]
                
                # add back in 
                df <- insert_columns(df, cur_dx, subset(temp, select=-n))
                
                # recode NAs in charlson column as 0
                df[which(is.na(df$charlson)),"charlson"] <- 0

                # Check for myocardial infarction. #
                df$mi <- pmax(df$mi, (df$charlson == 1)*1)

                # Check for peripheral vascular disease. #
                df$pvd <- pmax(df$pvd, (df$charlson == 2)*1)

                # Check for dementia. #
                df$dementia <- pmax(df$dementia, (df$charlson == 3)*1)

                # Check for chronic obstructive pulmonary disease. #
                df$copd <- pmax(df$copd, (df$charlson == 4)*1)

                # Check for rheumotologic disease. #
                df$rheum <- pmax(df$rheum, (df$charlson == 5)*1)

                # Check for mild liver disease. #
                df$mld <- pmax(df$mld, (df$charlson == 6)*1)

                # Check for moderate to severe liver disease. #
                df$sld <- pmax(df$sld, (df$charlson == 7)*1)

                # Check for mild to moderate diabetes mellitus. #
                df$mdm <- pmax(df$mdm, (df$charlson == 8)*1)

                # Check for chronic complications of diabetes mellitus. #
                df$ccdm <- pmax(df$ccdm, (df$charlson == 9)*1)

                # Check for renal disease. #
                df$renal <- pmax(df$renal, (df$charlson == 10)*1)

                # Check for any malignancy. #
                df$any_mal <- pmax(df$any_mal, (df$charlson == 11)*1)

                # Check for metastatic solid tumor. #
                df$mst <- pmax(df$mst, (df$charlson == 12)*1)

                # Check for AIDS. #
                df$aids <- pmax(df$aids, (df$charlson == 13)*1)

                # Drop variable charlson added by the merge process. #
                df <- subset(df, select=-charlson)
            }
            if(px_cnt > 0) for(i in 1:px_cnt){

                # Generate name of current diagnosis code variable. #
                cur_dx <- paste(px_pre, i, sep="")

                # Get number of missing observations for the current diagnosis code variable. #
                missing <- sum(is.na(df[,cur_dx]))

                # Merge with Charlson procedure code table. #
                if(switch == 1) {
                  temp <-  merge(df[,c(cur_dx,"n")],cptab_s1, by.x = cur_dx, by.y="px", all.x=T, sort=F)
                } else if (switch == 2) {
                  temp <-  merge(df[,c(cur_dx,"n")], cptab_s2, by.x = cur_dx, by.y="px", all.x=T, sort=F)
                }
                
                #reorder after merge
                temp <- temp[order(temp$n)]
                
                #add back in 
                df <- insert_columns(df, cur_dx, subset(temp, select=-n))
                
                # recode NAs in charlson column as 0
                df[which(is.na(df$charlson)), "charlson"] <- 0

                # Check for myocardial infarction. #
                df$mi <- pmax(df$mi, (df$charlson == 1)*1)

                # Check for peripheral vascular disease. #
                df$pvd <- pmax(df$pvd, (df$charlson == 2)*1)

                # Check for dementia. #
                df$dementia <- pmax(df$dementia, (df$charlson == 3)*1)

                # Check for chronic obstructive pulmonary disease. #
                df$copd <- pmax(df$copd, (df$charlson == 4)*1)

                # Check for rheumotologic disease. #
                df$rheum <- pmax(df$rheum, (df$charlson == 5)*1)

                # Check for mild liver disease. #
                df$mld <- pmax(df$mld, (df$charlson == 6)*1)

                # Check for moderate to severe liver disease. #
                df$sld <- pmax(df$sld, (df$charlson == 7)*1)

                # Check for mild to moderate diabetes mellitus. #
                df$mdm <- pmax(df$mdm, (df$charlson == 8)*1)

                # Check for chronic complications of diabetes mellitus. #
                df$ccdm <- pmax(df$ccdm, (df$charlson == 9)*1)

                # Check for renal disease. #
                df$renal <- pmax(df$renal, (df$charlson == 10)*1)

                # Check for any malignancy. #
                df$any_mal <- pmax(df$any_mal, (df$charlson == 11)*1)

                # Check for metastatic solid tumor. #
                df$mst <- pmax(df$mst, (df$charlson == 12)*1)

                # Check for AIDS. #
                df$aids <- pmax(df$aids, (df$charlson == 13)*1)

                # Drop variable charlson added by the merge process. #
                df <- subset(df, select=-charlson)
            }
            
            #these need work
            # # ?Set mild liver disease to 0 if moderate to severe liver disease is 1. #
            # df$mld = pmax(df$mld, (df$sld == 1) )
            df$mld <- 0# ifelse(df$sld == 1, 0, 1) # correct logic?
            #
            # # Set mild to moderate diabetes mellitus to 0 if chronic complications of diabetes mellitus is TRUE. #
            # if(df$ccdm == 1) df$mdm <- 0
            df$mdm <- 0# ifelse(df$ccdm ==1, 0, 1) # ?
            #
            # # Set any malignancy to FALSE if metastatic solid tumor is TRUE. #
            # if(df$mst == 1) df$any_mal <- 0
            df$any_mal <-0# ifelse(df$mst ==1, 0, 1) #?

            # Calculate number of Charlson comorbidities present. #
            df$charlcnt = df$mi + df$pvd + df$dementia + df$copd + df$rheum + df$mld + df$sld + 
                df$mdm + df$ccdm + df$renal + df$any_mal + df$mst + df$aids

            # Calculate Charlson score. #
            df$charlval = df$mi + df$pvd + df$dementia + df$copd + df$rheum + df$mld + df$mdm + 
                (df$renal*2) + (df$ccdm*2) + (df$any_mal*2) + (df$sld*3) + (df$mst*6) + (df$aids*6)

            subset(df,select = -n)
}
    
    
    
    
    












