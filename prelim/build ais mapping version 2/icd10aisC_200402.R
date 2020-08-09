#######################################################################
#
#      R PROGRAMS TO EXTRACT INJURY DATA 
#           FROM TQIP PUF (FORMERLY NTDB) AND FROM NIS
#           AND ANALYZE ROCMAX OPTIONS FOR ICDPIC-R
#
#      PART C: FROM EFFECTS OF INJURY SEVERITY, ASSIGN OPTIMAL AIS CUTOFFS
#              CREATE TABLE TO USE IN ICDPIC-R
#              David Clark, March-April 2020
#                     
########################################################################


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
require(glmnet)



#2
#IMPORT DATA WITH MAXIMUM EFFECT IN EACH BODY REGION FOR EACH PERSON
#IMPLEMENT "GREEDY" ALGORITHM TO SEEK OPTIMAL CUTPOINTS FOR SUM OF SQUARES

#  d1<-read_csv("Y:/Data_Event/NTDB/Analytic_Steps/tqip2017cm_effects.csv")
  d1<-read_csv("Y:/Data_Event/NTDB/Analytic_Steps/nis2016cm_effects.csv")
#  d1<-read_csv("Y:/Data_Event/NTDB/Analytic_Steps/tqip2017base_effects.csv")
#  d1<-read_csv("Y:/Data_Event/NTDB/Analytic_Steps/nis2016base_effects.csv")

d3<-filter(d1,idseq==1)    

#Define a function to return a triangular random variate
#   with range a to b and mode m.  
trirandom<-function(a,b,m) {
  k=(m-a)/(b-a)
  ru01=runif(1)
  rtri01k=if_else(ru01<=k,sqrt(k*ru01),(1-sqrt((1-k)*(1-ru01))))
  rtriabm=a+(b-a)*rtri01k
  return(rtriabm)
}
#Define a function to return a beta random variate
#   with range a to b, mode m, and "convexity" c (>1 is more peaked).
betarandom<-function(a,b,m,c) {
  k=(m-a)/(b-a)
  a2=c
  a1=a2*(k/(1-k))
  rbeta01k=rbeta(1,a1,a2)
  rbetaabm=a+(b-a)*rbeta01k
  return(rbetaabm)
}

d4<-d3

sink("x200225.txt",split=T)
starttime=Sys.time()

cut12=.162
cut23=.331
cut34=.752
cut45=1.32
bestroc=.5
i=0

while(i<1000) {
  
  i=i+1
  convexity=floor(i/100)+1
  old12=cut12
  old23=cut23
  old34=cut34
  old45=cut45
  if(i%%4==1) {
    cut12=trirandom(-4,cut23,cut12)
  }
  if(i%%4==2) {
    cut23=trirandom(cut12,cut34,cut23)
  }
  if(i%%4==3) {
    cut34=trirandom(cut23,cut45,cut34)
  }
  if(i%%4==0) {
    cut45=trirandom(cut34,4,cut45)
  }
  
  d4<-mutate(d4,a=case_when(
    amax>=-9 & amax<cut12 ~ 1,
    amax>=cut12 & amax<cut23 ~ 2,
    amax>=cut23 & amax<cut34 ~ 3,
    amax>=cut34 & amax<cut45 ~ 4,
    amax>=cut45 ~ 5,
    TRUE ~ 0
  ) )
  d4<-mutate(d4,c=case_when(
    cmax>=-9 & cmax<cut12 ~ 1,
    cmax>=cut12 & cmax<cut23 ~ 2,
    cmax>=cut23 & cmax<cut34 ~ 3,
    cmax>=cut34 & cmax<cut45 ~ 4,
    cmax>=cut45 ~ 5,
    TRUE ~ 0
  ) )
  d4<-mutate(d4,e=case_when(
    emax>=-9 & emax<cut12 ~ 1,
    emax>=cut12 & emax<cut23 ~ 2,
    emax>=cut23 & emax<cut34 ~ 3,
    emax>=cut34 & emax<cut45 ~ 4,
    emax>=cut45 ~ 5,
    TRUE ~ 0
  ) )
  d4<-mutate(d4,f=case_when(
    fmax>=-9 & fmax<cut12 ~ 1,
    fmax>=cut12 & fmax<cut23 ~ 2,
    fmax>=cut23 & fmax<cut34 ~ 3,
    fmax>=cut34 & fmax<cut45 ~ 4,
    fmax>=cut45 ~ 5,
    TRUE ~ 0
  ) )
  d4<-mutate(d4,h=case_when(
    hmax>=-9 & hmax<cut12 ~ 1,
    hmax>=cut12 & hmax<cut23 ~ 2,
    hmax>=cut23 & hmax<cut34 ~ 3,
    hmax>=cut34 & hmax<cut45 ~ 4,
    hmax>=cut45 ~ 5,
    TRUE ~ 0
  ) )
  d4<-mutate(d4,s=case_when(
    smax>=-9 & smax<cut12 ~ 1,
    smax>=cut12 & smax<cut23 ~ 2,
    smax>=cut23 & smax<cut34 ~ 3,
    smax>=cut34 & smax<cut45 ~ 4,
    smax>=cut45 ~ 5,
    TRUE ~ 0
  ) )
  d4<-mutate(d4,a2=a^2)
  d4<-mutate(d4,c2=c^2)
  d4<-mutate(d4,e2=e^2)
  d4<-mutate(d4,f2=f^2)
  d4<-mutate(d4,h2=h^2)
  d4<-mutate(d4,s2=s^2)
  d4<-mutate(d4,sumsquares=a2+c2+e2+f2+h2+s2)
  
  testroc<-roc(d4$died,d4$sumsquares,quiet=T)
  newroc=as.numeric(auc(testroc))
  
  if(newroc<=bestroc) {
    cut12=old12
    cut23=old23
    cut34=old34
    cut45=old45
  }
  if(newroc>bestroc){
    print(c("i:",i,"Cuts:",format(cut12,digits=3),format(cut23,digits=3),format(cut34,digits=3),
            format(cut45,digits=3),"AUC:",format(newroc,digits=4)))
    bestroc=newroc
  }
  if(i%%100==0){
    print(c("i:",i))
  }
  
} #while


endtime=Sys.time()
endtime-starttime
sink(NULL)


#Best for TQIPcm: .165, .335, .750, 1.26:  R-squared Sum Squares = .8560, ISS = .8567
#Best for NIScm: -.0638, .102, .400, .817; R-squared sum squares = .7472, ISS = .7479
#Best for TQIPbase: .241, .423, .766, 1.25; R-squared sum squares = .8445, ISS = .8403
#Best for NISbase:  .0266, .207, .449, .891; R-squared sum squares = .7275, ISS = .7279


#3
#FOR EACH DATABASE, VERIFY SUM OF SQUARES AND CALCULATE ISS

cut12=-.0638
cut23=.102
cut34=.4
cut45=.817


d5<-mutate(d3,a=case_when(
  amax>-9 & amax<cut12 ~ 1,
  amax>=cut12 & amax<cut23 ~ 2,
  amax>=cut23 & amax<cut34 ~ 3,
  amax>=cut34 & amax<cut45 ~ 4,
  amax>=cut45 ~ 5,
  TRUE ~ 0
) )
d5<-mutate(d5,c=case_when(
  cmax>-9 & cmax<cut12 ~ 1,
  cmax>=cut12 & cmax<cut23 ~ 2,
  cmax>=cut23 & cmax<cut34 ~ 3,
  cmax>=cut34 & cmax<cut45 ~ 4,
  cmax>=cut45 ~ 5,
  TRUE ~ 0
) )
d5<-mutate(d5,e=case_when(
  emax>-9 & emax<cut12 ~ 1,
  emax>=cut12 & emax<cut23 ~ 2,
  emax>=cut23 & emax<cut34 ~ 3,
  emax>=cut34 & emax<cut45 ~ 4,
  emax>=cut45 ~ 5,
  TRUE ~ 0
) )
d5<-mutate(d5,f=case_when(
  fmax>-9 & fmax<cut12 ~ 1,
  fmax>=cut12 & fmax<cut23 ~ 2,
  fmax>=cut23 & fmax<cut34 ~ 3,
  fmax>=cut34 & fmax<cut45 ~ 4,
  fmax>=cut45 ~ 5,
  TRUE ~ 0
) )
d5<-mutate(d5,h=case_when(
  hmax>-9 & hmax<cut12 ~ 1,
  hmax>=cut12 & hmax<cut23 ~ 2,
  hmax>=cut23 & hmax<cut34 ~ 3,
  hmax>=cut34 & hmax<cut45 ~ 4,
  hmax>=cut45 ~ 5,
  TRUE ~ 0
) )
d5<-mutate(d5,s=case_when(
  smax>-9 & smax<cut12 ~ 1,
  smax>=cut12 & smax<cut23 ~ 2,
  smax>=cut23 & smax<cut34 ~ 3,
  smax>=cut34 & smax<cut45 ~ 4,
  smax>=cut45 ~ 5,
  TRUE ~ 0
) )
d5<-mutate(d5,a2=a^2)
d5<-mutate(d5,c2=c^2)
d5<-mutate(d5,e2=e^2)
d5<-mutate(d5,f2=f^2)
d5<-mutate(d5,h2=h^2)
d5<-mutate(d5,s2=s^2)
d5<-mutate(d5,sumsquares=a2+c2+e2+f2+h2+s2)
skim(d5,a,c,e,f,h,s,sumsquares)
roc(d5$died,d5$sumsquares)
tabyl(d5,sumsquares) %>%
  adorn_pct_formatting(digits=2)


#Calculate actual ISS
d6<-gather(d5,a2,c2,e2,f2,h2,s2,key="reg",value="regmax")
d6<-group_by(d6,INC_KEY)
d6<-arrange(d6,desc(regmax))
d6<-mutate(d6,regorder=row_number())
d6<-mutate(d6,xiss=ifelse(regorder<=3,cumsum(regmax),0))
d6<-mutate(d6,riss=max(xiss))
d6<-ungroup(d6)
d6<-filter(d6,regorder==1)
d6<-select(d6,-reg,-regmax,-regorder,-xiss)

#Compare results to ISS in registry data
d7<-mutate(d6,risscat=case_when(
  riss>0 & riss<=8 ~ "01-08",
  riss>8 & riss<=15 ~ "09-15",
  riss>15 & riss<=24 ~ "16-24",
  riss>24 & riss<=40 ~ "25-40",
  riss>40 & riss<=49 ~ "41-49",
  riss>49 & riss<=75 ~ "50-75",
  TRUE ~ "Unk"
) )
d7<-mutate(d7,zisscat=case_when(
  ISSAIS>0 & ISSAIS<=8 ~ "01-08",
  ISSAIS>8 & ISSAIS<=15 ~ "09-15",
  ISSAIS>15 & ISSAIS<=24 ~ "16-24",
  ISSAIS>24 & ISSAIS<=40 ~ "25-40",
  ISSAIS>40 & ISSAIS<=49 ~ "41-49",
  ISSAIS>49 & ISSAIS<=75 ~ "50-75",
  TRUE ~ "Unk"
) )

d7<-mutate(d7,ISSAIS=ifelse(ISSAIS==-2,NaN,ISSAIS))

roc(d7$died,d7$ISSAIS)
roc(d7$died,d7$riss)
t<-tabyl(d7,zisscat,died) 
t2<-adorn_percentages(t,"row")  
t3<-adorn_pct_formatting(t2, digits=2)
t4<-adorn_ns(t3,"front")
t4
t<-tabyl(d7,risscat,died) 
t2<-adorn_percentages(t,"row")  
t3<-adorn_pct_formatting(t2, digits=2)
t4<-adorn_ns(t3,"front")
t4
t5<-tabyl(d7,zisscat,risscat)
t5
t6<-adorn_percentages(t5,"all")  
t7<-adorn_pct_formatting(t6, digits=2)
t8<-adorn_ns(t7,"front")
t8

##################################################################################






#4
#CREATE TABLE OF DIAGNOSIS CODES WITH EFFECTS AND AIS SCORES
#  Use optimal AIS cutpoints for each source, as determined above

rm(list=ls())
# d1<-read_csv("tqip2017cm_effects.csv")
  d1<-read_csv("nis2016cm_effects.csv")
#  d1<-read_csv("tqip2017base_effects.csv")
#  d1<-read_csv("nis2016base_effects.csv")


#Identify and remove duplicates, drop unnecessary variables
d2<-group_by(d1,dx)
d2<-mutate(d2,dxseq=row_number())
d2<-mutate(d2,dxrep=max(dxseq))
d2<-ungroup(d2)
d3<-filter(d2,dxseq==1)
d3<-arrange(d3,dx)
d3<-select(d3,dx,issbr,effect,ridge_int,dxrep)

####################################################
#  ENTER APPROPRIATE CUTPOINTS AND ASSIGN AIS VALUES
####################################################

#Best for TQIPcm: .165, .335, .750, 1.26:  R-squared Sum Squares = .8560, ISS = .8567
#Best for NIScm: -.0638, .102, .400, .817; R-squared sum squares = .7472, ISS = .7479
#Best for TQIPbase: .241, .423, .766, 1.25; R-squared sum squares = .8445, ISS = .8403
#Best for NISbase:  .0266, .207, .449, .891; R-squared sum squares = .7275, ISS = .7279
cut12=-.0638
cut23=0.102
cut34=0.400
cut45=0.817

#Assign AIS values
d4<-mutate(d3,ais=case_when(
  effect>-99 & effect<cut12 ~ 1,
  effect>=cut12 & effect<cut23 ~ 2,
  effect>=cut23 & effect<cut34 ~ 3,
  effect>=cut34 & effect<cut45 ~ 4,
  effect>=cut45 ~ 5,
  TRUE ~ 0
) )


#########################################
#  ONLY RUN THE APPROPRIATE SECTION BELOW
#########################################

#Rename diagnosis code type as appropriate and save results

d5<-rename(d4,icdcm=dx)
d5<-rename(d5,TQIPeffect=effect)
d5<-rename(d5,TQIPint=ridge_int)
d5<-rename(d5,TQIPais=ais)
d5<-rename(d5,TQIPbr=issbr)
#Add back codes that prematurely specify mortality outcome
#Use results from codes modified in Section A3a above
d5temp<-filter(d5,str_sub(icdcm,1,3)=="S06" & str_sub(icdcm,7,7)=="9")
d5temp6<-mutate(d5temp,icdcm=str_c(str_sub(icdcm,1,6),"6A"))
d5temp7<-mutate(d5temp,icdcm=str_c(str_sub(icdcm,1,6),"7A"))
d5temp8<-mutate(d5temp,icdcm=str_c(str_sub(icdcm,1,6),"8A"))
d5temp678<-bind_rows(list(d5temp6,d5temp7,d5temp8))
d6<-bind_rows(list(d5,d5temp678))
write_csv(d6,"tqip2017cm_ais.csv")

d5<-rename(d4,icdcm=dx)
d5<-rename(d5,NISeffect=effect)
d5<-rename(d5,NISint=ridge_int)
d5<-rename(d5,NISais=ais)
d5<-rename(d5,NISbr=issbr)
#Add back codes that prematurely specify mortality outcome
#Use results from codes modified in Section A3a above
d5temp<-filter(d5,str_sub(icdcm,1,3)=="S06" & str_sub(icdcm,7,7)=="9")
d5temp6<-mutate(d5temp,icdcm=str_c(str_sub(icdcm,1,6),"6A"))
d5temp7<-mutate(d5temp,icdcm=str_c(str_sub(icdcm,1,6),"7A"))
d5temp8<-mutate(d5temp,icdcm=str_c(str_sub(icdcm,1,6),"8A"))
d5temp678<-bind_rows(list(d5temp6,d5temp7,d5temp8))
d6<-bind_rows(list(d5,d5temp678))
write_csv(d6,"nis2016cm_ais.csv")

d5<-rename(d4,icdbase=dx)
d5<-rename(d5,TQIPeffect=effect)
d5<-rename(d5,TQIPint=ridge_int)
d5<-rename(d5,TQIPais=ais)
d5<-rename(d5,TQIPn=dxrep)
d5<-rename(d5,TQIPbr=issbr)
write_csv(d5,"tqip2017base_ais.csv")

d5<-rename(d4,icdbase=dx)
d5<-rename(d5,NISeffect=effect)
d5<-rename(d5,NISint=ridge_int)
d5<-rename(d5,NISais=ais)
d5<-rename(d5,NISn=dxrep)
d5<-rename(d5,NISbr=issbr)
write_csv(d5,"nis2016base_ais.csv")


#5
#MERGE AIS TABLES
#Create two working tables for ICDPIC-R

rm(list=ls())
d1<-read_csv("tqip2017cm_ais.csv")
d2<-read_csv("nis2016cm_ais.csv")
d3<-full_join(d1,d2,by="icdcm")  

d1<-read_csv("tqip2017base_ais.csv")
d2<-read_csv("nis2016base_ais.csv")
d3<-full_join(d1,d2,by="icdbase")  

#Compare TQIP and NIS results - quite different!
tabyl(d3,TQIPais,NISais)
tabyl(d3,TQIPbr,NISbr)

#If AIS or Body Region missing in one database, borrow from the other
d4<-mutate(d3,TQIPais_mod=if_else(is.na(TQIPais),NISais,TQIPais))
d4<-mutate(d4,NISais_mod=if_else(is.na(NISais),TQIPais,NISais))
d4<-mutate(d4,TQIPbr_mod=if_else(is.na(TQIPbr),NISbr,TQIPbr))
d4<-mutate(d4,NISbr_mod=if_else(is.na(NISbr),TQIPbr,NISbr))

tabyl(d4,TQIPais,NISais)
tabyl(d4,TQIPais_mod,NISais_mod)
tabyl(d4,TQIPbr_mod,NISbr_mod)           

#Sort and save results for application to ICDPIC-R
d4<-arrange(d4,icdcm)
write_csv(d4,"TQIP_NIS_ais_cm.csv")

d4<-arrange(d4,icdbase)
write_csv(d4,"TQIP_NIS_ais_base.csv")










#6
#TEST WHAT WORKS BEST FOR OUT-OF-SAMPLE PREDICTION


#  d1<-read_csv("Y:/Data_Event/NTDB/Analytic_Steps/tqip2017cm_effects.csv")
#  d1<-read_csv("Y:/Data_Event/NTDB/Analytic_Steps/nis2016cm_effects.csv")
# d1<-read_csv("Y:/Data_Event/NTDB/Analytic_Steps/tqip2017base_effects.csv")
  d1<-read_csv("Y:/Data_Event/NTDB/Analytic_Steps/nis2016base_effects.csv")

d2<-read_csv("TQIP_NIS_ais_cm.csv")

#Specify whether the data are ICD-10-CM or basic ICD-10
d2<-rename(d2,dx=icdcm)
d2<-rename(d2,dx=icdbase)

d3<-full_join(d1,d2,by="dx")  


#Specify which reference database is to be used for calculations
d3<-mutate(d3,ais=NISais)
d3<-mutate(d3,Pint=NISint)
d3<-mutate(d3,Peff=NISeffect)
d3<-mutate(d3,issbr=NISbr_mod)


#Obtain maximum AIS for each person and body region
d4<-group_by(d3,INC_KEY,issbr)
d4<-mutate(d4,a0=ifelse(issbr=="Abdomen",max(ais),0))
d4<-mutate(d4,c0=ifelse(issbr=="Chest",max(ais),0))
d4<-mutate(d4,e0=ifelse(issbr=="Extremities",max(ais),0))
d4<-mutate(d4,f0=ifelse(issbr=="Face",max(ais),0))
d4<-mutate(d4,h0=ifelse(issbr=="Head/Neck",max(ais),0))
d4<-mutate(d4,s0=ifelse(issbr=="General",max(ais),0))
d4<-ungroup(d4)
d4<-ungroup(d4)
d4<-group_by(d4,INC_KEY)
d4<-mutate(d4,idseq=row_number())
d4<-mutate(d4,amax=max(a0))
d4<-mutate(d4,cmax=max(c0))
d4<-mutate(d4,emax=max(e0))
d4<-mutate(d4,fmax=max(f0))
d4<-mutate(d4,hmax=max(h0))
d4<-mutate(d4,smax=max(s0))
d4<-ungroup(d4)
d4<-arrange(d4,INC_KEY,dx)

#calculate ISS and ROC
d5<-mutate(d4,a=amax)
d5<-mutate(d5,c=cmax)
d5<-mutate(d5,e=emax)
d5<-mutate(d5,f=fmax)
d5<-mutate(d5,h=hmax)
d5<-mutate(d5,s=smax)
d5<-select(d5,INC_KEY,dx,died,ais,a,c,e,f,h,s)

d5<-mutate(d5,a2=a^2)
d5<-mutate(d5,c2=c^2)
d5<-mutate(d5,e2=e^2)
d5<-mutate(d5,f2=f^2)
d5<-mutate(d5,h2=h^2)
d5<-mutate(d5,s2=s^2)
d5<-select(d5,-a,-c,-e,-f,-h,-s)

#Get one observation per person
d5<-group_by(d5,INC_KEY)
d5<-mutate(d5,idseq=row_number())
d5<-ungroup(d5)
d5<-filter(d5,idseq==1)

#Calculate ISS
d6<-gather(d5,a2,c2,e2,f2,h2,s2,key="reg",value="regmax")
d6<-group_by(d6,INC_KEY)
d6<-arrange(d6,desc(regmax))
d6<-mutate(d6,regorder=row_number())
d6<-mutate(d6,xiss=ifelse(regorder<=3,cumsum(regmax),0))
d6<-mutate(d6,riss=max(xiss))
d6<-ungroup(d6)
d6<-filter(d6,regorder==1)
d6<-select(d6,-ais,-idseq,-reg,-regmax,-regorder,-xiss)
d6<-arrange(d6,INC_KEY,dx)

roc(d6$died,d6$riss)

#Calculate Pmort 
d7<-group_by(d3,INC_KEY)
d7<-mutate(d7,idseq=row_number())
d7<-mutate(d7,sumeff=sum(Peff))
d7<-ungroup(d7)
d7<-filter(d7,idseq==1)
d7<-mutate(d7,xb=sumeff+Pint)
d7<-mutate(d7,Pmort=1/(1+exp(xb)))

roc(d7$died,d7$Pmort)

