# for debuging...
# set.seed(1)
# codes <- c()
# n <- 5
# df <- data.frame(dx1 = sample(ntab_s1$dx, n),
#                  dx2 = sample(ntab_s1$dx, n),
#                  dx3 = sample(i10_map_min$dx, n),
#                  dx4 = sample(i10_map_max$dx, n),
#                  dx5 = sample(etab_s1$dx, n),
#                  dx6 = sample(i10_ecode$dx, n),
#                  dx7 = sample(i10_map_max$dx, n),
#                  dx8 = sample(.select_i10_data("NIS", "cm")$dx, n),
#                  dx9 = sample(.select_i10_data("NIS", "base")$dx, n),
#                  dx10 = sample(.select_i10_data("TQIP", "cm")$dx, n),
#                  dx11 = sample(.select_i10_data("TQIP", "base")$dx, n))
#
# df <- data.frame(dx1 = c("S027","S067","S327"))
# result <- cat_trauma(df,"dx", icd10 = "base")
#
# df
# dx_pre="dx"
# calc_method = 1
# icd10 <- T
# i10_iss_method <- "empirical"
