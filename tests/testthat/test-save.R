test_that("save works", {

  df <- data.frame(x = 1:2)
  names(df) <- "abc,def"

  df |>
    save_df("test.sdfs") |>
    expect_error()
})
