
##############################################################################################################
###  helper functions: load these first.
##############################################################################################################

### mapping functions for mchanism and intent codes (used with e-codes)
map_mechmaj <- function(m){
      if(length(m)>1) print(m)
      if(m==0) "Cut/pierce"
      else if(m==1) "Drowning/submersion"
      else if(m==2) "Fall"
      else if(m==3) "Fire/burn"
      else if(m==4) "Firearm"
      else if(m==5) "Machinery"
      else if(m==6) "Motor vehicle traffic"
      else if(m==7) "Pedal cyclist, other"
      else if(m==8) "Pedestrian, other"
      else if(m==9) "Transport, other"
      else if(m==10) "Natural/environmental"
      else if(m==11) "Overexertion"
      else if(m==12) "Poisoning"
      else if(m==13) "Struck by, against"
      else if(m==14) "Suffocation"
      else if(m==15) "Other specified, classifiable"
      else if(m==16) "Other specified, not elsewhere classifiable"
      else if(m==17) "Unspecified"
      else if(m==18) "Adverse effects"
      else NA
}

map_mechmin <- function(m, min){
      if(is.na(min) || is.na(m)) NA
      else if(m==3 && min == 0) "Fire/flame"
      else if(m==3 && min == 1) "Hot object/substance"
      else if(m==6 && min == 0) "Occupant"
      else if(m==6 && min == 1) "Motorcyclist"
      else if(m==6 && min == 2) "Pedal cyclist"
      else if(m==6 && min == 3) "Pedestrian"
      else if(m==6 && min == 4) "Unspecified"
      else if(m==10 && min == 0) "Bites and stings"
      else if(m==18 && min == 0) "Medical care"
      else if(m==18 && min ==1) "Drugs"
      else NA
}

map_intent <- function(i){
      if(i==0) "Unintentional"
      else if(i==1) "Self-inflicted"
      else if(i==2) "Assault"
      else if(i==3) "Undetermined"
      else if(i==4) "Other"
      else NA
}

map_issbr <- function(i){
      if(i==1) "Head/Neck"
      else if(i==2) "Face"
      else if(i==3) "Chest"
      else if(i==4) "Abdomen"
      else if(i %in% c(5,6)) "General"
      else "Unknown"
}

