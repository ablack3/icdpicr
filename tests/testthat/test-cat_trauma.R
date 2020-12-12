library(dplyr)

test_that("cat_trauma runs", {
  out <- cat_trauma(head(injury, 100), "dx")
  expect_equal(nrow(out), 100)
  expect_s3_class(out, "data.frame")
})

test_that("niss != riss", {
  # count the rows where niss and riss differ
  out <- injury %>%
    filter(!is.na(dx10)) %>%
    head(100) %>%
    # slice(6000:10000) %>%
    cat_trauma("dx") %>%
    mutate(rownum = row_number()) %>%
    filter(niss != riss) %>%
    tibble()

  expect_equal(nrow(out), 71)
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
    expect_equal(c(3, 57, 3))
})

test_that("TQIP and NIS give different results", {


  df <- data.frame(INC_KEY = 10000120, died = 0, dx1 = "S00.81XA",
                   dx2 = "S22.081A", dx3 = "S22.41XA", dx4 = "S30.811A", dx5 = "S32.2XXA",
                   dx6 = "S42.251A", dx7 = "S42.291A", dx8 = "S82.231A", dx9 = "S82.491A")

  df_out_nis <- cat_trauma(df, "dx", i10_iss_method = "roc_max_NIS")
  df_out_tqip <- cat_trauma(df, "dx", i10_iss_method = "roc_max_TQIP")

  expect_equal(df_out_nis$riss, 14)
  expect_equal(df_out_nis$niss, 14)
  expect_equal(df_out_tqip$riss, 9)
  expect_equal(df_out_tqip$niss, 9)
})



