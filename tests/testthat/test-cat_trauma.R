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

  expect_equal(nrow(out), 53)
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
    expect_equal(c(3, 34, 3))
})


