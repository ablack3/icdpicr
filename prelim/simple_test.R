library(icdpicr)

df_in <- read.table(header = T, text = "
ident    dx1     dx2     dx3
31416   800.1   959.9   E910.9
31417   800.24  410.0   NA
")
df_out <- cat_trauma(df_in, "dx")

df_in <- read.table(header = T, text = "
dx1      dx2      dx3
S80.812A S82.235A   S82235A
S42.454A Y04.0XXA   S82.235
S06.360A     NA     S8223
S00.93XA     NA     S822
S52.011A  S02.0XXB  S82
")
write.csv(df_in, "smalltest.csv", row.names = F)

?cat_trauma
df_out <- cat_trauma(df_in, "dx", i10_iss_method = "roc_max")



#I should add more complete unit tests here

df_in <- read.table(header = T, text = "
dx1      dx2      dx3
S80.812A S82.235A V03.10XA
S42.454A Y04.0XXA   NA
S06.360A     NA     NA
S00.93XA     NA     NA
S52.011A  S02.0XXB     NA
")

head(icdpicr::i10_ecode)

# table not exported
df_out <- cat_trauma(df_in, "dx")



n <- 5
df <- data.frame(dx1 = sample(ntab_s1$dx, n),
                 dx2 = sample(ntab_s1$dx, n),
                 dx3 = sample(i10_map_min$dx, n),
                 dx4 = sample(i10_map_max$dx, n),
                 dx5 = sample(etab_s1$dx, n),
                 dx6 = sample(i10_ecode$dx, n),
                 dx7 = sample(i10_map_max$dx, n),
                 dx8 = sample(.select_i10_data("NIS", "cm")$dx, n),
                 dx9 = sample(.select_i10_data("NIS", "base")$dx, n),
                 dx10 = sample(.select_i10_data("TQIP", "cm")$dx, n),
                 dx11 = sample(.select_i10_data("TQIP", "base")$dx, n))

result <- cat_trauma(df,"dx")




