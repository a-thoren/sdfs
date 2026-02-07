#' Loads a data.frame
#'
#' @param file_name name of file to load
#'
#' @returns data.frame
load <- function(file_name) {
  f <- file(file_name, "rb")

  structure(
    list(
      connection = f,
      index = read_index(f),
      file_name = file_name
    ),
    class = "sdfs"
  )
}

#' @export
#' @importFrom dplyr select
select.sdfs <- function(.data, ...) {
  cols <- .data$columns
  if (is.null(cols)) {
    cols <- .data$index$names
  }

  selected <- tidyselect::vars_select(cols, ...)

  .data$columns <- selected

  .data
}

#' @export
#' @importFrom dplyr collect
collect.sdfs <- function(x, ...) {

  res <- read_columns(
    f = x$connection,
    index = x$index,
    columns = purrr::pluck(
      x,
      "columns",
      .default = purrr::pluck(x, "index", "names")
    )
  )

  close(x$connection)
  res
}

#' @export
#' @importFrom dplyr rename
rename.sdfs <- function(.data, ...) {
  renamed <- tidyselect::vars_rename(
    .vars = purrr::pluck(.data, "index", "names"),
    ...
  )

  purrr::assign_in(.data, c("index", "names"), names(renamed))

}


#' Reads the index of a file
#'
#' The index consists of
#' \enumerate{
#'  \item number of rows of the data.frame
#'  \item number of columns of the data.frame
#'  \item types of the data.frame
#'  \item indices for each column of the data.frame in the file
#'  \item column names of the data.frame
#'  \item the offset where the index starts
#' }
#'
#' @param f file connection
#'
#' @returns index
read_index <- function(f) {
  checkmate::assert_class(f, c("file", "connection"))

  seek(f, where = -32, origin = "end")

  index_offset <- readBin(f, "raw", n = 32) |>
    packBits("integer")
  file_name <- summary(f)$description
  file_size <- file.info(file_name)$size

  seek(f, where = index_offset)
  index_raw <- readBin(f, what = "raw", n = file_size - index_offset - 32)

  nrow <- packBits(index_raw[1:32], "integer")
  ncol <- packBits(index_raw[33:64], "integer")
  types_end <- 64 + 32 * ncol
  types <- packBits(index_raw[65:types_end], "integer")
  index_end <- types_end + 32 * ncol
  indices <- packBits(index_raw[(types_end + 1):index_end], "integer")
  names <- rawToChar(index_raw[(index_end + 1):length(index_raw)]) |>
    stringr::str_split_1(",")

  list(
    nrow = nrow,
    ncol = ncol,
    types = types,
    indices = indices,
    names = names,
    index_offset = index_offset
  )

}

read_columns <- function(f, index, columns) {
  checkmate::assert_class(f, c("file", "connection"))
  checkmate::assert_character(columns)
  checkmate::assert_subset(columns, index$names)

  columns_idx <- which(index$names %in% columns)

  starts <- index$indices
  ends <- c(index$indices[-1], index$index_offset) - 1
  lens <- ends - starts + 1

  dplyr::bind_cols(
    start = starts,
    len = lens,
    type = index$types
  ) |>
    dplyr::slice(columns_idx) |>
    purrr::pmap(read_column, n = index$nrow, f = f) |>
    purrr::set_names(purrr::keep_at(index$names, columns_idx)) |>
    dplyr::bind_cols()


}

read_column <- function(f, start, len, type, n) {
  seek(f, where = start)
  readBin(f, what = "raw", n = len) |>
    decode(type, n)
}
