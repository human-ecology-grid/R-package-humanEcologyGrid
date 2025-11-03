#' Obtain the Cross-Sectional (Latest) Human Ecology Grid Data
#'
#' @param dataset  the dataset descriptor (see `[dataset()])
#' @param arrow    whether to load the data as an Arrow dataset or as a tibble
#' @param ...      reserved for future extensions
#'
#' Cross-sectional data contains the most recent year coverage for each data module.
#'
#' @examples
#' \dontrun{
#' data_descriptor <- humanEcologyGrid::dataset(resolution = 300, download = TRUE)
#' humanEcologyGrid::latest_data(data_descriptor)
#' }
#'
#' @export
latest_data <- function(dataset, ..., arrow = getOption("human.ecology.grid.arrow", FALSE)) {
  rlang::check_dots_empty()
  check_dataset_downloaded(dataset)
  data_uri <- get_dataset_cached_uri(dataset)

  # open the dataset
  data <- arrow::open_dataset(file.path(data_uri, "cross-section-latest.parquet"))
  if (!isTRUE(arrow)) data <- tibble::as_tibble(data)

  data
}
