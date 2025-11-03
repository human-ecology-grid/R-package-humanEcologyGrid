#' Obtain the yearly Human Ecology Grid Data
#'
#' @param dataset  the dataset descriptor (see `[dataset()])
#' @param year     the year to load data for (numeric vector)
#' @param arrow    whether to load the data as an Arrow dataset or as a tibble
#' @param ...      reserved for future extensions
#'
#' @examples
#' \dontrun{
#' data_descriptor <- humanEcologyGrid::dataset(resolution = 300, download = TRUE)
#' humanEcologyGrid::yearly_data(data_descriptor, year = 1995)
#' }
#'
#' @export
yearly_data <- function(dataset, ..., year, arrow = getOption("human.ecology.grid.arrow", FALSE)) {
  rlang::check_dots_empty()
  check_dataset_downloaded(dataset)
  data_uri <- get_dataset_cached_uri(dataset)

  # open the dataset
  data <- arrow::open_dataset(file.path(data_uri, "yearly"))

  if (!missing(year)) data <- arrow:::filter.Dataset(data, year %in% {{year}})
  if (!isTRUE(arrow)) data <- tibble::as_tibble(data)

  data
}
