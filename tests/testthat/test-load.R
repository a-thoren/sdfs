test_that("read_index works", {
  df <- data.frame(
    x = 1:5,
    y = 2:6
  )

  f <- withr::local_tempfile()

  save_df(df, f)

  f_con <- withr::local_connection(file(f, "rb"))

  index <- read_index(f_con)

  expect_equal(
    index,
    list(
      nrow = 5,
      ncol = 2,
      types = c(1, 1),
      indices = c(5, 32 * 5 + 5),
      names = c("x", "y"),
      index_offset = 2 * 5 * 32 + 5
    )
  )
})

test_that("load works", {

  df <- data.frame(
    x = 1:5,
    abc = c(1.1, pi, 0, -1, NA),
    what_The_HHHHEEEEEck = c(TRUE, FALSE, NA, TRUE, FALSE),
    wow = c("abc", "hhhh", "öööööööö", NA, "1")
  )

  f <- withr::local_tempfile()

  save_df(df, f)

  expect_true(file.exists(f))

  df_2 <- load(f) |>
    dplyr::collect()

  expect_equal(
    df_2,
    tibble::tibble(
      x = 1:5,
      abc = c(1.1, pi, 0, -1, NA),
      what_The_HHHHEEEEEck = c(TRUE, FALSE, NA, TRUE, FALSE),
      wow = c("abc", "hhhh", "öööööööö", NA, "1")
    )
  )

  load(f) |>
    dplyr::select("x", "abc") |>
    dplyr::collect() |>
    expect_equal(
      dplyr::select(df, "x", "abc") |>
        tibble::as_tibble()
    )

  load(f) |>
    dplyr::collect() |>
    expect_equal(tibble::as_tibble(df))

})
