df_in <- read.table(header = T, text = "
ident    dx1     dx2     dx3
                    31416   800.1   959.9   E910.9
                    31417   800.24  410.0   NA
                    ")
df_out <- cat_trauma(df_in, "dx")

df_in <- read.table(header = T, text = "
dx1      dx2      dx3
S80.812A S82.235A V03.10XA
S42.454A Y04.0XXA   NA
S06.360A     NA     NA
S00.93XA     NA     NA
S52.011A  S02.0XXB     NA
")

df_out <- cat_trauma(df_in, "dx")
