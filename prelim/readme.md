# Prelim - package development code
This folder contains the files used to create the icdpicr package.

The goal of this project is to update the icdpic Stata application to work with ICD10 codes and implement
the program in R.

This program needs to 
- map ICD 9 to AIS and body region using the original ntab_s1 table from Stata
- map ICD 9 E-codes to mechanism (major & minor) and intent using the original etab_s1 table from Stata
- map ICD 10 codes to AIS and body region using the i10_map_max and i10_map_min tables derived using the CMS GEM
- map ICD 10 codes to AIS and body region using the i10_map_emp table derived using Dave's empirical method on the NTDB data
- map ICD 10 injury mechanism codes to mechanism and intent using the i10_ecode table derived from a CMS table


## ntab_s1 & etab_s1
1. download table used in original icdpic https://ideas.repec.org/c/boc/bocode/s457028.html
1. make changes suggested by users

## i10_map_max & i10_map_min
1. download GEM from https://www.cms.gov/medicare/coding/icd10/2016-icd-10-cm-and-gems.html
1. Use the GEM to map icd 10 to icd9 
1. Use ntab_s1 to map to AIS and body region
1. conflicts on ISS handled by max or min rule. Give user the option to choose which.
1. conflicts on body region were handled using keywords in the text description for the ICD 10 code

## i10_map_emp
1. import and format Daves mapping table (icd10ais.xls) derived from NTDB data

## i10_ecode
1. Create icd10 e-code mapping using i10_transcode.pdf from CDC and regex.

## Steps
1. create the data tables described above and save them in the lookup tables folder as csv files
1. load the csv files and save them as an rda object for use in the package (create sysdata.R)
1. Rewrite trauma module from STATA program in R 


## document

## unit tests?







