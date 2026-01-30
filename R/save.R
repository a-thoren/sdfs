#' @describeIn write writes the header
#' @param version version, currently not used
write_header <- function(file_counter, version = 1) {

  header <- c(charToRaw("sdfs"), as.raw(version))

  writeBin(
    header,
    file_counter$file
  )

  increase_counter(file_counter, length(header))

}

#' @describeIn write increases the counter in the file counter
#' @param add the amount to increase the counter by
increase_counter <- function(file_counter, add) {
  checkmate::assert_integerish(add, lower = 1, len = 1)
  file_counter$counter <- file_counter$counter + add
  file_counter
}


#' Writes data to the data
#'
#' @param x object to write, should be a raw
#' @param file_counter the file counter, contains a file connection and a
#' counter of how many raws have been written to it
#'
#' @returns `file_counter` with increased counter
write <- function(x, file_counter) {
  checkmate::assert_raw(x)
  writeBin(
    x,
    file_counter$file
  )
  increase_counter(file_counter, length(x))
}

#' @describeIn write writes a column of a data.frame
write_column <- function(x, file_counter) {
  encoded <- encode(x)
  write(encoded, file_counter)
}


#' Writes the index
#'
#' @param df the data.frame being saved
#' @param file_counter file counter
#' @param indices indices of each column in the file
#'
#' @returns `file_counter` with increased counter
write_index <- function(df, file_counter, indices) {
  index <- c(
    nrow(df),
    ncol(df),
    get_types(df),
    indices
  ) |>
    intToBits()

  index <- c(
    index,
    encode_character_simple(names(df))
  )

  write(index, file_counter)
}


#' Saves a data.frame
#'
#' @param df data.frame to save
#' @param file_name name of file
#'
#' @returns nothing
#' @export
save_df <- function(df, file_name) {

  checkmate::assert_data_frame(
    x = df,
    types = c("logical", "integer", "numeric", "character")
  )

  if (any(stringr::str_detect(names(df), ","))) {
    cli::cli_abort(
      "{.arg df} may not have names that contain the character {.value ,}"
    )
  }

  f <- withr::local_connection(file(file_name, "wb"))

  file_counter <- write_header(list(file = f, counter = 0))
  indices <- list()

  file_counter <- purrr::reduce2(
    df,
    names(df),
    \(file_counter, x, nm) {
      indices <<- c(indices, purrr::set_names(file_counter$counter, nm))
      write_column(x, file_counter)
    },
    .init = file_counter
  )

  index_offset <- file_counter$counter
  file_counter <- write_index(df, file_counter, indices)

  write(
    intToBits(index_offset),
    file_counter
  )

  invisible(NULL)

}

#' @describeIn write_index numeric encoding of a data.frames types
get_types <- function(df) {
  purrr::map(
    df,
    \(x) switch(
      class(x),
      "integer" = 1,
      "numeric" = 2,
      "logical" = 3,
      "character" = 4
    )
  ) |>
    unlist()
}
