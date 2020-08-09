# ICDPIC-R Lookup tables


This directory contains comma separated value (csv) files of the lookup tables used in the ICDPIC-R program

All of the tables in the original ICDPIC Stata program can be found at http://ideas.repec.org/c/boc/bocode/s457028.html

**etab_s1.csv**
A crosswalk between ICD 9 e-codes and major mechanism, minor mechanism and intent.
This is identical to the original ICDPIC e-code table.

**ntab_s1.csv**
This table maps ICD 9 n-codes to severity (AIS) and body region (1-6).
It has been modified from the original ICDPIC table in two ways as suggested by DiBartolomeo (2010) and Fleischman (2017): 850.11 maps to severity 2 instead of 1 and 862.8 maps to severity 5 instead of 6. In addition the barrell and apc columns were dropped.

**i10_map_min.csv**
This table maps ICD-10-CM diagnosis coeds to severity and body region using the CMS general equivalence mapping (GEM) to first map to ICD 9 and then use the mapping in n_tab_s1.csv. This table handles conflicts in severity by mapping ICD 10 codes that are mapped to less than one severity code to the minimum severity.

**i10_map_max.csv**
This table is identical to i10_map_min.csv except that when an ICD 10 code is mapped to 
less than one severity code the maximum severity is used.

**i10_etab.csv**
This table maps ICD 10 external cause of injury codes (e-codes) to major mechanism, minor mechanism and intent. This table was generated from the file icd_transcode.pdf published by the CDC. It is the ICD 10 analog to etab_s1 table.

**i10_map_roc_nis.csv**
This table maps  ICD-10  and ICD-10-CM diagnosis codes to severity and body region using a prediction model trained on the National Trauma Data Bank. The objective function was maximizing the area under the ROC curve. It is referred to as the roc_max method.

**i10_map_roc_tqip.csv**
This table maps ICD-10-CM diagnosis codes to severity and body region using a prediction model trained on the National Trauma Data Bank. The objective function was maximizing the area under the ROC curve. It is referred to as the roc_max method.


