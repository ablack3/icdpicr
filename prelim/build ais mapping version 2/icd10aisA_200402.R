#########################################################################
#
#      R PROGRAMS TO EXTRACT INJURY DATA 
#           FROM TQIP PUF (FORMERLY NTDB) AND FROM NIS
#           AND ANALYZE ROCMAX OPTIONS FOR ICDPIC-R
#
#      PART A: EXTRACT DATA
#
#           This part mostly translated from Stata used in earlier version
#           David Clark, 2019-2020
#                     
##########################################################################


#1
#CLEAR WORKSPACE, SET WORKING DIRECTORY,
#LOAD REQUIRED PACKAGES, IF NOT LOADED ALREADY
rm(list=ls())
setwd("Y:/Data_Event/NTDB/Analytic_Steps")  
require(tidyverse)
require(skimr)
require(janitor)
require(broom)
require(pROC)
require(lme4)
require(ggplot2)


#2a
#GET TQIP DATA

#Import 2017 TQIP data and identify ER or inpatient mortality  
  d0<-read_csv("Y:/Data_Event/NTDB/Original_Source/PUF AY 2017/CSV/PUF_TRAUMA.csv")  
  d1<-rename(d0,INC_KEY=inc_key,ISSAIS=ISS_05)
#Note "DEATHINED" means no Vital Signs on ED admission, but some do survive
  tabyl(d1,HOSPDISCHARGEDISPOSITION,DEATHINED)
  tabyl(d1,HOSPDISCHARGEDISPOSITION_BIU,DEATHINED)
#Only counted as "died" if discharge dispositon is explicitly dead 
#    or explicitly missing after having no vital signs on ED admission
  d2<-mutate(d1,died=case_when(
       HOSPDISCHARGEDISPOSITION==5 ~ 1,
       (DEATHINED==1 & HOSPDISCHARGEDISPOSITION_BIU==1) ~ 1,
       TRUE ~ 0
       ))
  tabyl(d2,died,DEATHINED)
  d3<-select(d2,INC_KEY,died,ISSAIS)
  write_csv(d3,"tqipmort2017.csv")  

#Obtain ICD-10 codes for each TQIP patient
  d0<-read_csv("Y:/Data_Event/NTDB/Original_Source/PUF AY 2017/CSV/PUF_ICDDIAGNOSIS.csv")
  d1<-rename(d0,INC_KEY=inc_key,icdcm=ICDDIAGNOSISCODE)

#Identify duplicates
  d2<-group_by(d1,INC_KEY,icdcm)
  d2<-mutate(d2,dxseq=row_number())
  d2<-mutate(d2,dxrep=max(dxseq))
  d2<-ungroup(d2)
  d2test<-filter(d2,dxrep>1)
  d2test<-arrange(d2test,INC_KEY,icdcm,dxseq,dxrep)
  #View(d2test)
#Drop duplicates
  d3<-filter(d2,dxseq==1)
  d3<-select(d3,INC_KEY,icdcm)
  d3<-arrange(d3,INC_KEY,icdcm)
  write_csv(d3,"tqipdcode2017.csv")
 
#Merge TQIP patient diagnoses and mortality, save result
  d1<-read_csv("tqipdcode2017.csv")
  d2<-read_csv("tqipmort2017.csv")  
  d3<-full_join(d1,d2,by="INC_KEY")  
  write_csv(d3,"tqipmerged2017.csv")
  
  
#2b  
#GET NIS DATA

#Import previously extracted 2016 NIS cases with any diagnosis starting with S or T
#This pre-extraction was done using Stata, but is surely possible in R as well  
  d0<-read_csv("y:/Data_Event/HCUP_NIS/Converted_Files/core2016ST.csv")
  
#Note curious fact that only New England states use odd numbers for KEY_NIS
  d0<-mutate(d0,odd=KEY_NIS%%2)
  tabyl(d0,odd,HOSP_DIVISION)
  
#Include only admissions that were not elective and originated in Emergency Department
  tabyl(d0,ELECTIVE,HCUP_ED
  d0<-filter(d0,ELECTIVE==0,HCUP_ED!=0)
  tabyl(d0,ELECTIVE,HCUP_ED)
  
#Restrict to variables of interest
#Rename mechanism of injury variables   
  d1<-select(d0,KEY_NIS,DIED,I10_DX1,I10_DX2,I10_DX3,I10_DX4,I10_DX5,I10_DX6,
             I10_DX7,I10_DX8,I10_DX9,I10_DX10,I10_DX10,I10_DX12,I10_DX13,I10_DX14,
             I10_DX15,I10_DX16,I10_DX17,I10_DX18,I10_DX19,I10_DX20,I10_DX21,I10_DX22,
             I10_DX23,I10_DX24,I10_DX25,I10_DX26,I10_DX27,I10_DX28,I10_DX29,I10_DX30,  
             I10_ECAUSE1,I10_ECAUSE2,I10_ECAUSE3,I10_ECAUSE4)
  d1<-rename(d1,I10_DX31=I10_ECAUSE1)
  d1<-rename(d1,I10_DX32=I10_ECAUSE2)
  d1<-rename(d1,I10_DX33=I10_ECAUSE3)
  d1<-rename(d1,I10_DX34=I10_ECAUSE4)   
  
#Reshape to "long" format
  d2<-gather(d1,I10_DX1,I10_DX2,I10_DX3,I10_DX4,I10_DX5,I10_DX6,
             I10_DX7,I10_DX8,I10_DX9,I10_DX10,I10_DX10,I10_DX12,I10_DX13,I10_DX14,
             I10_DX15,I10_DX16,I10_DX17,I10_DX18,I10_DX19,I10_DX20,I10_DX21,I10_DX22,
             I10_DX23,I10_DX24,I10_DX25,I10_DX26,I10_DX27,I10_DX28,I10_DX29,I10_DX30,  
             I10_DX31,I10_DX32,I10_DX33,I10_DX34,key="original",value="rawdx")
  
#Reformat ICD10 codes to have a decimal point in the 4th position
#Rename key field  
  d3<-mutate(d2,dxpart1=str_sub(rawdx,1,3))
  d3<-mutate(d3,dxpart2=str_sub(rawdx,4,7))
  d3<-mutate(d3,icdcm=str_c(dxpart1,".",dxpart2))
  d3<-rename(d3,INC_KEY=KEY_NIS)
  d3<-rename(d3,died=DIED)
  
#Identify duplicates
  d3a<-group_by(d3,INC_KEY,icdcm)
  d3a<-mutate(d3a,dxseq=row_number())
  d3a<-mutate(d3a,dxrep=max(dxseq))
  d3a<-ungroup(d3a)
  d3atest<-filter(d3a,dxrep>1)
  d3atest<-arrange(d3atest,INC_KEY,icdcm,dxseq,dxrep)
  #View(d3atest)
#Drop duplicates (mostly NA)
  d3b<-filter(d3a,dxseq==1)
  d3b<-select(d3b,INC_KEY,icdcm,died)
  d3b<-arrange(d3b,INC_KEY,icdcm)

#Save result
  write_csv(d3b,"nisreshaped2016.csv")  
  
  
#3a 
#PREPARE TQIP OR NIS DATA FOR ANALYSIS    
    
  #d0<-read_csv("tqipmerged2017.csv")
  d0<-read_csv("nisreshaped2016.csv")  

#Restrict to cases with at least one valid ICD10CM Anatomic Injury Code
#  as defined by National Trauma Data Standard
#This includes fracture codes ending in B or C (denoting open fractures)
  d4<-mutate(d0,validcode=case_when(
    str_sub(icdcm,1,1)=="S" & str_sub(icdcm,8,8)=="A" ~ 1,
    str_sub(icdcm,1,1)=="S" & str_sub(icdcm,8,8)=="B" ~ 1,
    str_sub(icdcm,1,1)=="S" & str_sub(icdcm,8,8)=="C" ~ 1,
    str_sub(icdcm,1,1)=="T" & str_sub(icdcm,2,3)=="07" ~ 1,
    str_sub(icdcm,1,1)=="T" & str_sub(icdcm,2,3)=="14" ~ 1,
    str_sub(icdcm,1,1)=="T" & str_sub(icdcm,2,2)=="2" & str_sub(icdcm,3,3)!="9" & str_sub(icdcm,8,8)=="A" ~ 1,
    str_sub(icdcm,1,1)=="T" & str_sub(icdcm,2,3)=="30" ~ 1,
    str_sub(icdcm,1,1)=="T" & str_sub(icdcm,2,3)=="31" ~ 1,
    str_sub(icdcm,1,1)=="T" & str_sub(icdcm,2,3)=="32" ~ 1,
    str_sub(icdcm,1,5)=="T79.A" & str_sub(icdcm,8,8)=="A" ~ 1, 
    TRUE ~ 0
    ) )

#Inspect whether valid codes properly identified
  d4test<-group_by(d4,icdcm)
  d4test<-mutate(d4test,dxseq=row_number())
  d4test<-filter(d4test,dxseq==1)
  d4test<-ungroup(d4test)
  d4test<-arrange(d4test,icdcm)
  #View(d4test)

#Identify valid cases (with at least one valid code, and died==0 or 1)
#  and discard invalid cases and codes  
  d5<-group_by(d4,INC_KEY)
  d5<-mutate(d5,idseq=row_number())
  d5<-mutate(d5,validcase=max(validcode))
  d5<-ungroup(d5)
  d5test<-filter(d5,idseq==1)
  tabyl(d5test,validcase)
  d6<-filter(d5,validcase==1,validcode==1)
  d6<-filter(d6,died>=0)

#Reassign a few codes with explicit outcome (survival or death)
#  Replacing "6", "7", or "8" in position 7 with "9"
  d7<-mutate(d6,explicit=case_when(
    str_sub(icdcm,1,3)=="S06" & str_sub(icdcm,7,7)=="6" ~ 1,
    str_sub(icdcm,1,3)=="S06" & str_sub(icdcm,7,7)=="7" ~ 1,
    str_sub(icdcm,1,3)=="S06" & str_sub(icdcm,7,7)=="8" ~ 1,
    TRUE ~ 0
    ) )
  d7<-mutate(d7,icdcm=ifelse(explicit, str_c(str_sub(icdcm,1,6),"9A"), icdcm))

#Identfy duplicates created by above reassignment
  d8<-group_by(d7,INC_KEY,icdcm)
  d8<-mutate(d8,dxseq=row_number())
  d8<-mutate(d8,dxrep=max(dxseq))
  d8<-ungroup(d8)
  tabyl(d8,dxseq)
  d8test<-filter(d8,dxrep>1)
  d8test<-arrange(d8test,INC_KEY,icdcm,dxseq,dxrep)
  #View(d8test)
#Drop these duplicates
  d9<-filter(d8,dxseq==1)

#Add body region for each valid diagnosis
  d10<-mutate(d9,issbr=case_when(
    str_sub(icdcm,1,3)=="S00" & str_sub(icdcm,5,5)=="1" ~ "Face",
    str_sub(icdcm,1,3)=="S00" & str_sub(icdcm,5,5)=="2" ~ "Face",
    str_sub(icdcm,1,3)=="S00" & str_sub(icdcm,5,5)=="3" ~ "Face",
    str_sub(icdcm,1,3)=="S00" & str_sub(icdcm,5,5)=="4" ~ "Face",
    str_sub(icdcm,1,3)=="S00" & str_sub(icdcm,5,5)=="5" ~ "Face",
    str_sub(icdcm,1,3)=="S01" & str_sub(icdcm,5,5)=="1" ~ "Face",
    str_sub(icdcm,1,3)=="S01" & str_sub(icdcm,5,5)=="2" ~ "Face",
    str_sub(icdcm,1,3)=="S01" & str_sub(icdcm,5,5)=="3" ~ "Face",
    str_sub(icdcm,1,3)=="S01" & str_sub(icdcm,5,5)=="4" ~ "Face",
    str_sub(icdcm,1,3)=="S01" & str_sub(icdcm,5,5)=="5" ~ "Face",
    str_sub(icdcm,1,3)=="S02" & str_sub(icdcm,5,5)=="1" ~ "Face",
    str_sub(icdcm,1,3)=="S02" & str_sub(icdcm,5,5)=="2" ~ "Face",
    str_sub(icdcm,1,3)=="S02" & str_sub(icdcm,5,5)=="3" ~ "Face",
    str_sub(icdcm,1,3)=="S02" & str_sub(icdcm,5,5)=="4" ~ "Face",
    str_sub(icdcm,1,3)=="S02" & str_sub(icdcm,5,5)=="5" ~ "Face",
    str_sub(icdcm,1,3)=="S02" & str_sub(icdcm,5,5)=="6" ~ "Face",
    str_sub(icdcm,1,3)=="S03" & str_sub(icdcm,5,5)=="0" ~ "Face",
    str_sub(icdcm,1,3)=="S03" & str_sub(icdcm,5,5)=="1" ~ "Face",
    str_sub(icdcm,1,3)=="S03" & str_sub(icdcm,5,5)=="2" ~ "Face",
    str_sub(icdcm,1,3)=="S03" & str_sub(icdcm,5,5)=="3" ~ "Face",
    str_sub(icdcm,1,3)=="S03" & str_sub(icdcm,5,5)=="4" ~ "Face",
    str_sub(icdcm,1,3)=="S03" & str_sub(icdcm,5,5)=="5" ~ "Face",
    str_sub(icdcm,1,3)=="S05"   ~ "Face",
    str_sub(icdcm,1,5)=="S07.0" ~ "Face",
    str_sub(icdcm,1,5)=="S08.1" ~ "Face",
    str_sub(icdcm,1,5)=="S09.2" ~ "Face",
    str_sub(icdcm,1,2)=="S0" ~ "Head/Neck",
    str_sub(icdcm,1,2)=="S1" ~ "Head/Neck",
    str_sub(icdcm,1,2)=="S2" ~ "Chest",
    str_sub(icdcm,1,2)=="S3" ~ "Abdomen",
    str_sub(icdcm,1,2)=="S4" ~ "Extremities",
    str_sub(icdcm,1,2)=="S5" ~ "Extremities",
    str_sub(icdcm,1,2)=="S6" ~ "Extremities",
    str_sub(icdcm,1,2)=="S7" ~ "Extremities",
    str_sub(icdcm,1,2)=="S8" ~ "Extremities",
    str_sub(icdcm,1,2)=="S9" ~ "Extremities",
    str_sub(icdcm,1,5)=="T24.2" ~ "Extremities",
    str_sub(icdcm,1,5)=="T79.A" ~ "Extremities",
    str_sub(icdcm,1,1)=="T" ~ "General",
    TRUE ~ "None"  
    ) )
tabyl(d10,issbr)
  
#Discard temporary analytic variables, and save results
  d11<-select(d10,-validcode,-idseq,-validcase,-explicit,-dxseq,-dxrep)
  #write_csv(d11,"tqip2017cm.csv")
  write_csv(d11,"nis2016cm.csv")
  

#3b  
#PREPARE A SIMILAR FILE WITH BASIC ICD-10 CODES
#(Used for death certificates in USA and for clinical data in other countries)
  rm(list=ls())
  #d1<-read_csv("tqip2017cm.csv")
  d1<-read_csv("nis2016cm.csv")
  
  d2<-mutate(d1,icdbase=str_sub(icdcm,1,5))

  #Identify and add extra digit to denote open fractures
  d2<-mutate(d2,icdbase=case_when(
       str_sub(icdcm,8,8)=="B" ~ str_c(icdbase,"1",sep=""),
       str_sub(icdcm,8,8)=="C" ~ str_c(icdbase,"1",sep=""),
       TRUE ~ icdbase
       ) )
  d2test<-mutate(d2,test=if_else(str_sub(icdbase,6,6)==1,1,0))
  d2test<-filter(d2test,test==1)
  
#Identfy duplicates created by collapsing diagnoses above
  d3<-group_by(d2,INC_KEY,icdbase)
  d3<-mutate(d3,dxseq=row_number())
  d3<-mutate(d3,dxrep=max(dxseq))
  d3<-ungroup(d3)
  tabyl(d3,dxseq)
  d3test<-filter(d3,dxrep>1)
  d3test<-arrange(d3test,INC_KEY,icdcm,icdbase,dxseq,dxrep)
  #View(d3test)
#Drop these duplicates
  d4<-filter(d3,dxseq==1)
  d4test<-filter(d4,str_length(icdcm)==3)
  #View(d4test)
  
#Drop temporary analytic variables, and save result  
  d4<-select(d4,-icdcm,-dxseq,-dxrep)
  #write_csv(d4,"tqip2017base.csv")
  write_csv(d4,"nis2016base.csv")
  

