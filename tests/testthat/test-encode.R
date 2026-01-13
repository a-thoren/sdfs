test_that("encoding and decoding works", {
  encode_logical(c(TRUE, FALSE, NA)) |>
    expect_equal(
      as.raw(c(1, 0, 2))
    )

  res <- encode_numeric(c(1, 0.5, NA))
  expect_length(res, 64 * 3)
  expect_snapshot(res)

  res <- encode_integer(c(0L, 1L, NA))
  expect_length(res, 32 * 3)
  expect_snapshot(res)

  res <- encode_character(c("abc", "defg", NA))
  expect_snapshot(res)
  res |>
    decode_character(3) |>
    expect_equal(c("abc", "defg", NA))
})
