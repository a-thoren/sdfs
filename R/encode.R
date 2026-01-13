#' Encodes values
#'
#' @param x object to encode
#'
#' @returns raw representation of `x`
encode <- function(x) {

  encode_function <- switch(
    class(x),
    "logical" = encode_logical,
    "numeric" = encode_numeric,
    "integer" = encode_integer,
    "character" = encode_character
  )

  encode_function(x)

}

#' @describeIn encode encodes logicals
encode_logical <- function(x) {
  checkmate::assert_logical(x)

  out <- rep(as.raw(2), length(x))
  out[x] <- as.raw(1)
  out[!x] <- as.raw(0)

  out
}

#' @describeIn encode encodes numerics
encode_numeric <- function(x, ...) {
  checkmate::assert_numeric(x)
  numToBits(x)
}

#' @describeIn encode encodes integers
encode_integer <- function(x, ...) {
  checkmate::assert_integer(x)
  intToBits(x)
}

#' @describeIn encode encodes characters
encode_character <- function(x) {
  checkmate::assert_character(x)
  lengths <- x |>
    nchar() |>
    purrr::modify_if(is.na, ~ 0)
  offsets <- cumsum(lengths) + 1
  offsets <- c(1, offsets[-length(offsets)])
  c(
    intToBits(offsets),
    intToBits(lengths),
    charToRaw(paste0(purrr::discard(x, is.na), collapse = ""))
  )
}

#' @describeIn encode encodes characters into a comma-separated representation
encode_character_simple <- function(x) {
  x |>
    purrr::modify_if(is.na, ~ "") |>
    paste0(collapse = ",") |>
    charToRaw()
}

#' Decodes raw values
#'
#' @param x values to decode
#' @param type type of encoded values
#' @param n number of values in `x`, only used when decoding characters
#'
#' @returns decoded values
decode <- function(x, type, n) {

  decode_function <- switch(
    type,
    decode_integer,
    decode_numeric,
    decode_logical,
    decode_character
  )

  decode_function(x, n)

}

#' @describeIn decode decodes logicals
decode_logical <- function(x, ...) {
  checkmate::assert_raw(x)

  out <- rep(NA, length(x))
  out[x == as.raw(1)] <- TRUE
  out[x == as.raw(0)] <- FALSE
  out
}

#' @describeIn decode decodes numerics
decode_numeric <- function(x, ...) {
  checkmate::assert_raw(x)
  packBits(x, "double")
}

#' @describeIn decode decodes integers
decode_integer <- function(x, ...) {
  checkmate::assert_raw(x)
  packBits(x, "integer")
}

#' @describeIn decode decodes characters
decode_character <- function(x, n) {
  offsets_end <- 32 * n
  offsets <- packBits(x[1:offsets_end], "integer")
  lengths_end <- offsets_end + 32 * n
  lengths <- packBits(x[(offsets_end + 1):lengths_end], "integer")
  chars <- rawToChar(x[(lengths_end + 1):length(x)])
  starts <- offsets
  ends <- starts + lengths - 1
  purrr::map2(
    starts,
    ends,
    x = chars,
    substr
  ) |>
    unlist() |>
    purrr::modify_if(~ .x == "", ~ NA)
}

#' @describeIn decode decodes simple character representation
decode_character_simple <- function(x, ...) {
  x |>
    rawToChar() |>
    stringr::str_split_1(",") |>
    purrr::modify_if(~ .x == "", ~ NA)
}
