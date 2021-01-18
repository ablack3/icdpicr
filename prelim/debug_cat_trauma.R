# for debuging...
set.seed(1)
codes <- c()
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

df <- data.frame(dx1 = c("S027","S067","S327"))
result <- cat_trauma(df,"dx", icd10 = "base")

df
dx_pre="dx"
calc_method = 1
icd10 <- T
i10_iss_method <- "roc_max_NIS"

df <- head(injury)
df

usethis::use_testthat()
usethis::use_test()



system.time({
      res <- injury %>%
            # head(1000) %>%
            cat_trauma(dx_pre = "dx") %>%
            tibble()
})

res %>%
      ggplot(aes(riss, mortality_prediction)) +
      geom_point() +
      geom_smooth()

res %>%
      ggplot(aes(niss, riss)) +
      geom_point() +
      geom_smooth()

res %>%
  rename(p = mortality_prediction) %>%
  mutate(mcat = case_when(p>=0 & p<.1 ~ 1,
                             p>=.1 & p<.2 ~ 2,
                             p>=.2 & p<.3 ~ 3,
                             p>=.3 & p<.4 ~ 4,
                             p>=.4 & p<.5 ~ 5,
                             p>=.5 & p<.6 ~ 6,
                             p>=.6 & p<.7 ~ 7,
                             p>=.7 & p<.8 ~ 8,
                             p>=.8 & p<.9 ~ 9,
                             p>=.9 & p<=1 ~10,
                             p>1 ~ 99)) %>%
      tabyl(mcat, died) %>%
      adorn_percentages("row") %>%
      adorn_pct_formatting(digits=2) %>%
      adorn_ns("front")

res$died
injury
cal <- caret::calibration(factor(1-died) ~ mortality_prediction, data = res)
plot(cal)
