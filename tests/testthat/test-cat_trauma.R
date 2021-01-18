library(dplyr)

test_that("cat_trauma runs", {
  out <- cat_trauma(head(injury, 100), "dx", T, "roc_max_NIS")
  expect_equal(nrow(out), 100)
  expect_s3_class(out, "data.frame")
})

test_that("niss != riss", {
  # count the rows where niss and riss differ
  out <- injury %>%
    filter(!is.na(dx10)) %>%
    head(100) %>%
    # slice(6000:10000) %>%
    cat_trauma("dx", T, "roc_max_NIS") %>%
    mutate(rownum = row_number()) %>%
    filter(niss != riss) %>%
    tibble()

  expect_equal(nrow(out), 69)
})


test_that("niss calculation is correct on a few rows", {
  data.in <- injury %>%
    head(10) %>%
    select(matches("dx")) %>%
    mutate_all(substr, 1, 5) %>%
    slice(c(7,8,10))

  cat_trauma(data.in, dx_pre =  "dx", icd10 = "base", i10_iss_method = "roc_max_NIS") %>%
    select(matches("sev|niss")) %>%
    pull(niss) %>%
    expect_equal(c(3, 66, 3))
})

test_that("TQIP and NIS give different results", {


  df <- data.frame(INC_KEY = 10000120, died = 0, dx1 = "S00.81XA",
                   dx2 = "S22.081A", dx3 = "S22.41XA", dx4 = "S30.811A", dx5 = "S32.2XXA",
                   dx6 = "S42.251A", dx7 = "S42.291A", dx8 = "S82.231A", dx9 = "S82.491A")

  df_base <- data.frame(INC_KEY = 10000120, died = 0, dx1 = "S00.81",
                   dx2 = "S22.08", dx3 = "S22.41", dx4 = "S30.81", dx5 = "S32.2",
                   dx6 = "S42.25", dx7 = "S42.29", dx8 = "S82.23", dx9 = "S82.49")

  df_out_nis_cm <- cat_trauma(df, "dx", icd10 = "cm", i10_iss_method = "roc_max_NIS")
  expect_equal(df_out_nis_cm$riss, 19)
  expect_equal(df_out_nis_cm$niss, 19)

  df_out_nis_base <- cat_trauma(df_base, "dx", icd10 = "base", i10_iss_method = "roc_max_NIS")
  expect_equal(df_out_nis_base$riss, 4)
  expect_equal(df_out_nis_base$niss, 4)

  df_out_tqip_cm <- cat_trauma(df, "dx", icd10 = "cm", i10_iss_method = "roc_max_TQIP")
  expect_equal(df_out_tqip_cm$riss, 9)
  expect_equal(df_out_tqip_cm$niss, 9)

  df_out_tqip_base <- cat_trauma(df_base, "dx", icd10 = "base", i10_iss_method = "roc_max_TQIP")
  expect_equal(df_out_tqip_base$riss, 1)
  expect_equal(df_out_tqip_base$niss, 1)
})



