
# this is the section that needs optimizing!
devtools::load_all()
etab <- rbind(etab_s1, i10_ecode)



df <- data.frame(key = 1:9,
           dx1 = c("R65.21", "S02.40EA", "R78.81", "K85.90", "Z68.44", "Z68.1", "Z79.4", "R33.9", "R09.02"),
           dx2 = c("R13.10", "N17.9", "R53.1", "R31.9", "Z92.241", "R29.810", "R33.9", "Q76.2", "Z90.710"),
           dx3 = c("Z90.13", "R56.9", "M71.22", "R13.10", "N17.9", "R04.0", "M19.90", "R00.0", "Z87.891"),
           dx4 = c("I10", "Z85.820", "J96.90", "G89.11", "K92.2", "Z85.43", "N39.0", "J96.90", "H35.00"),
           dx5 = c("G10", "J90", "Z66", "F32.9",  "K76.0", "Z85.3", "N18.9", "J69.0", "E88.09"),
           dx6 = c("J45.909", "R91.1", "E87.5", "F10.221", "Z51.5", "I82.4Z2", "N17.9", "E87.6", "J81.1"),
           dx7 = c("R50.9", "N39.0", "J44.9", "K57.30", "E87.2", "I10", "I10", "E83.51", "F05"),
           dx8 = c("I11.0", "I46.9", "R06.82", "G89.11", "K21.9", "E88.09", "E83.42", "E87.5", "E86.9"),
           dx9 = c("I10", "E83.42", "J86.9", "G81.94", "R00.1", "E83.39", "F32.9", "E83.52", "E87.3"), stringsAsFactors = F)

# get ecode column names
ecode_colnames <- paste0("ecode_", 1:4)

#create ecode columns
df[ , ecode_colnames] <- NA

# for each row extract the first 4 ecodes and add them to the e-code columns
# icd10 e-codes do not start with E.

# get a list of all ecodes (includes icd10 code if requested)
ecode_regex <- paste0("^", etab$dx, collapse = "|")

profvis::profvis({
df[ , ecode_colnames] <- t(apply(df, 1, function(row){
      # remove decimal
      row <- sub("\\.", "", row)
      # get all e codes using pattern matching
      row_ecodes <- stringr::str_extract(as.character(unlist(row)), ecode_regex)
      # row_ecodes <- grep(ecode_regex, as.character(unlist(row)), value = T) #1370
      # remove na values
      row_ecodes <- na.omit(row_ecodes)
      # save first 4 Ecodes
      row_ecodes[1:4]
}))

})
grep(ecode_regex, sub("\\.", "", as.character(df[6,-1])), value = T)

grep(ecode_regex, c("X600", "X53", "hi","Y82"), value = T)

as.matrix(df[,-1]) %>%
      t() %>%
      .[,]
      stringr::str_extract( ecode_regex)
{.}


