#######################################################################
#
#      R PROGRAMS TO EXTRACT INJURY DATA 
#           FROM TQIP PUF (FORMERLY NTDB) AND FROM NIS
#           AND ANALYZE ROCMAX OPTIONS FOR ICDPIC-R
#
#      PART B: USING RIDGE REGRESSION, RANK INJURY SEVERITY
#           David Clark, January 2021
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
#CONSTRUCT MATRICES FOR RIDGE REGRESSION
# (in pieces for larger datasets, due to memory restrictions)

#d0<-read_csv("tqip2017cm.csv")
d0<-read_csv("nis2016cm.csv")
d0<-rename(d0,dx=icdcm)

#d0<-read_csv("tqip2017base.csv")
#d0<-read_csv("nis2016base.csv")
#d0<-rename(d0,dx=icdbase)

#################################################################
#For tqipcm or niscm have to split up data in the following steps.
modno=9
#Create dummy observation with all diagnoses
# (so all pieces have the same columns)
ddummy<-group_by(d0,dx)
ddummy<-mutate(ddummy,dxseq=row_number())
ddummy<-ungroup(ddummy)
ddummy<-filter(ddummy,dxseq==1)
ddummy<-select(ddummy,INC_KEY,dx,died)
ddummy<-mutate(ddummy,died=0)
ddummy<-mutate(ddummy,INC_KEY=modno)
#Take 10% sample with mod(INC_KEY,10)=modno and append to dummy
dmod<-filter(d0,INC_KEY%%10==modno)
dmod<-bind_rows(ddummy,dmod)
dmod<-arrange(dmod,INC_KEY,dx)
################################################################

################################################################
#For basic ICD10 (not ICD10CM) just do the following
#dmod<-arrange(d0,INC_KEY,dx)
################################################################

#Convert mortality data to a vector with one observation per person
dmort<-group_by(dmod,INC_KEY)
dmort<-mutate(dmort,idseq=row_number())
dmort<-ungroup(dmort)
dmort<-filter(dmort,idseq==1)
dmort<-select(dmort,died)
matmort<-data.matrix(dmort)

#Convert diagnosis data to wide format and sparse logic (T/F) matrix
#  (This is the part that needs some memory and time)
ddx<-mutate(dmod,x=T)
ddx<-select(ddx,INC_KEY,dx,x)
ddx<-spread(ddx,dx,x,fill=F)
ddx<-select(ddx,-INC_KEY)

smatdx<-Matrix(as.matrix(ddx),sparse=TRUE)
smatdx[1:5,1:5]
object.size(ddx)    #  3.3GB for TQIPcm, 1.9GB for NIScm
object.size(smatdx) #  3.8MB for TQIPcm, 1.9MB for NIScm
rm(ddx,dmort)

#Save matrices named by source and modno (if needed)
write(matmort,"nis2016cm_matmort9",ncolumns=1)
writeMM(smatdx,"nis2016cm_smatdx9")

##############################################################
#For tqipcm and niscm have to recombine the pieces as follows
#Import partial data matrices
#FOR TQIP:
matmort0<-as.matrix(read.table("tqip2017cm_matmort0"))
smatdx0<-readMM("tqip2017cm_smatdx0")
matmort1<-as.matrix(read.table("tqip2017cm_matmort1"))
smatdx1<-readMM("tqip2017cm_smatdx1")
matmort2<-as.matrix(read.table("tqip2017cm_matmort2"))
smatdx2<-readMM("tqip2017cm_smatdx2")
matmort3<-as.matrix(read.table("tqip2017cm_matmort3"))
smatdx3<-readMM("tqip2017cm_smatdx3")
matmort4<-as.matrix(read.table("tqip2017cm_matmort4"))
smatdx4<-readMM("tqip2017cm_smatdx4")
matmort5<-as.matrix(read.table("tqip2017cm_matmort5"))
smatdx5<-readMM("tqip2017cm_smatdx5")
matmort6<-as.matrix(read.table("tqip2017cm_matmort6"))
smatdx6<-readMM("tqip2017cm_smatdx6")
matmort7<-as.matrix(read.table("tqip2017cm_matmort7"))
smatdx7<-readMM("tqip2017cm_smatdx7")
matmort8<-as.matrix(read.table("tqip2017cm_matmort8"))
smatdx8<-readMM("tqip2017cm_smatdx8")
matmort9<-as.matrix(read.table("tqip2017cm_matmort9"))
smatdx9<-readMM("tqip2017cm_smatdx9")
#FOR NIS
matmort0<-as.matrix(read.table("nis2016cm_matmort0"))
smatdx0<-readMM("nis2016cm_smatdx0")
matmort1<-as.matrix(read.table("nis2016cm_matmort1"))
smatdx1<-readMM("nis2016cm_smatdx1")
matmort2<-as.matrix(read.table("nis2016cm_matmort2"))
smatdx2<-readMM("nis2016cm_smatdx2")
matmort3<-as.matrix(read.table("nis2016cm_matmort3"))
smatdx3<-readMM("nis2016cm_smatdx3")
matmort4<-as.matrix(read.table("nis2016cm_matmort4"))
smatdx4<-readMM("nis2016cm_smatdx4")
matmort5<-as.matrix(read.table("nis2016cm_matmort5"))
smatdx5<-readMM("nis2016cm_smatdx5")
matmort6<-as.matrix(read.table("nis2016cm_matmort6"))
smatdx6<-readMM("nis2016cm_smatdx6")
matmort7<-as.matrix(read.table("nis2016cm_matmort7"))
smatdx7<-readMM("nis2016cm_smatdx7")
matmort8<-as.matrix(read.table("nis2016cm_matmort8"))
smatdx8<-readMM("nis2016cm_smatdx8")
matmort9<-as.matrix(read.table("nis2016cm_matmort9"))
smatdx9<-readMM("nis2016cm_smatdx9")

#Remove initial dummy row from each matrix
matmort0<-as.matrix(matmort0[-1,])
smatdx0<-smatdx0[-1,]
matmort1<-as.matrix(matmort1[-1,])
smatdx1<-smatdx1[-1,]
matmort2<-as.matrix(matmort2[-1,])
smatdx2<-smatdx2[-1,]
matmort3<-as.matrix(matmort3[-1,])
smatdx3<-smatdx3[-1,]
matmort4<-as.matrix(matmort4[-1,])
smatdx4<-smatdx4[-1,]
matmort5<-as.matrix(matmort5[-1,])
smatdx5<-smatdx5[-1,]
matmort6<-as.matrix(matmort6[-1,])
smatdx6<-smatdx6[-1,]
matmort7<-as.matrix(matmort7[-1,])
smatdx7<-smatdx7[-1,]
matmort8<-as.matrix(matmort8[-1,])
smatdx8<-smatdx8[-1,]
matmort9<-as.matrix(matmort9[-1,])
smatdx9<-smatdx9[-1,]

#Combine matrices and save 
matmort<-rbind(matmort0,matmort1,matmort2,matmort3,matmort4,matmort5,matmort6,matmort7,matmort8,matmort9)
smatdx<-rbind(smatdx0,smatdx1,smatdx2,smatdx3,smatdx4,smatdx5,smatdx6,smatdx7,smatdx8,smatdx9)
#FOR TQIP
write(matmort,"tqip2017cm_matmort",ncolumns=1)
writeMM(smatdx,"tqip2017cm_smatdx")
#FOR NIS
write(matmort,"nis2016cm_matmort",ncolumns=1)
writeMM(smatdx,"nis2016cm_smatdx")
####################################################################################


#3
#RIDGE REGRESSION

#Obtain mortality and diagnosis data in matrix format
matmort<-as.matrix(read.table("tqip2017cm_matmort"))
smatdx<-readMM("tqip2017cm_smatdx")
matmort<-as.matrix(read.table("nis2016cm_matmort"))
smatdx<-readMM("nis2016cm_smatdx")
matmort<-as.matrix(read.table("tqip2017base_matmort"))
smatdx<-readMM("tqip2017base_smatdx")
matmort<-as.matrix(read.table("nis2016base_matmort"))
smatdx<-readMM("nis2016base_smatdx")

#Convert ngTMatrix (logival) to dgCMatrix (numeric)
#Otherwise cv.glmnet doesn't work
smatdx=smatdx*1

#Determine optimal value of lambda by cross-validation
#Note: Ridge regression if alpha=0 (LASSO if alpha=1)
cvridge<-cv.glmnet(smatdx,matmort,family="binomial",alpha=0) 
plot(cvridge)
cvridge$lambda.min
log(cvridge$lambda.min)
#Get coefficients for model with lowest binomial deviance
mridge<-coef(cvridge,s="lambda.min")
ridge_eff<-as.data.frame(summary(mridge))
skim(ridge_eff)
intercept=ridge_eff[1,3]


#Obtain corresponding data in original format
#d0<-read_csv("tqip2017cm.csv")
d0<-read_csv("nis2016cm.csv")
d0<-rename(d0,dx=icdcm)
#d0<-read_csv("tqip2017base.csv")
#d0<-read_csv("nis2016base.csv")
#d0<-rename(d0,dx=icdbase)
d0<-arrange(d0,INC_KEY,dx)


#Extract corresponding list of all diagnoses in the data
d1<-group_by(d0,dx)
d1<-mutate(d1,dxseq=row_number())
d1<-ungroup(d1)
d1<-filter(d1,dxseq==1)
d1<-select(d1,dx)
d1<-arrange(d1,dx)
d1<-mutate(d1,dxseq=row_number())


#Line up rows correctly to merge data
head(mridge)
head(ridge_eff)
head(d1)
d2<-mutate(d1,i=dxseq+1)
head(d2)
ridge_eff2<-slice(ridge_eff,-1)
head(ridge_eff2)
d3<-full_join(d2,ridge_eff2,"i")
head(d3)

d4<-full_join(d0,d3,"dx")
d4<-group_by(d4,INC_KEY)
d4<-mutate(d4,idseq=row_number())
#Added following line 201002
d4<-mutate(d4,x=if_else(is.na(x),0,x))
d4<-mutate(d4,totridge=sum(x))
d4<-ungroup(d4)
d4<-mutate(d4,ridge_int=intercept)
d4<-arrange(d4,INC_KEY,dx)

#Explore results
d4test=filter(d4,idseq==1)
lm(died~totridge,data=d4test)
roc(d4test$died,d4test$totridge,quiet=T)

d4test<-mutate(d4test,xb=ridge_int+totridge)
d4test<-mutate(d4test,pdied=exp(xb)/(1+exp(xb)))
d4test<-mutate(d4test,pdiedcat=case_when(
      pdied<.1 ~ 5,
      pdied>=.1 & pdied<.2 ~ 15,
      pdied>=.2 & pdied<.3 ~ 25,
      pdied>=.3 & pdied<.4 ~ 35,
      pdied>=.4 & pdied<.5 ~ 45,
      pdied>=.5 & pdied<.6 ~ 55,
      pdied>=.6 & pdied<.7 ~ 65,
      pdied>=.7 & pdied<.8 ~ 75,
      pdied>=.8 & pdied<.9 ~ 85,
      pdied>=.9 ~ 95,
      TRUE ~ -1
      ))
t<-tabyl(d4test,pdiedcat,died)
t2<-adorn_percentages(t,"row")  
t3<-adorn_pct_formatting(t2, digits=2)
t4<-adorn_ns(t3,"front")
t4


#4
#Obtain maximum "effect" for each person and body region
d5<-rename(d4,effect=x)
d5<-group_by(d5,INC_KEY,issbr)
d5<-mutate(d5,a0=ifelse(issbr=="Abdomen",max(effect),-99))
d5<-mutate(d5,c0=ifelse(issbr=="Chest",max(effect),-99))
d5<-mutate(d5,e0=ifelse(issbr=="Extremities",max(effect),-99))
d5<-mutate(d5,f0=ifelse(issbr=="Face",max(effect),-99))
d5<-mutate(d5,h0=ifelse(issbr=="Head/Neck",max(effect),-99))
d5<-mutate(d5,s0=ifelse(issbr=="General",max(effect),-99))
d5<-ungroup(d5)
d5<-ungroup(d5)
d5<-group_by(d5,INC_KEY)
d5<-mutate(d5,idseq=row_number())
d5<-mutate(d5,amax=max(a0))
d5<-mutate(d5,cmax=max(c0))
d5<-mutate(d5,emax=max(e0))
d5<-mutate(d5,fmax=max(f0))
d5<-mutate(d5,hmax=max(h0))
d5<-mutate(d5,smax=max(s0))
d5<-ungroup(d5)

#Discard temporary analytic variables, and save results
d6<-select(d5,-dxseq,-i,-j,-a0,-c0,-e0,-f0,-h0,-s0)

write_csv(d6,"tqip2017cm_effects.csv")

write_csv(d6,"nis2016cm_effects.csv")

write_csv(d6,"tqip2017base_effects.csv")

write_csv(d6,"nis2016base_effects.csv")






