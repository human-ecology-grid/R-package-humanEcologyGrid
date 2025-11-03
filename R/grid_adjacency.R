#' Obtain the Human Ecology Grid Cell Adjacency Matrix
#'
#' @param dataset         the dataset descriptor (see `[dataset()])
#' @param data            a data frame containing a column `cell_id` with the cell IDs
#' @param ...             reserved for future extensions
#'
#'
#' If reference dataset `data` is provided, the returned matrix will match the
#' order of the cells in the data and drop all unused cells. This is useful when
#' modeling spatial autocorrelation with packages such as `brms`.
#'
#' @examples
#' \dontrun{
#' data_descriptor <- humanEcologyGrid::dataset(resolution = 300, download = TRUE)
#' # cells for Pacific-Centered maps
#' humanEcologyGrid::grid_adjacency(data_descriptor)
#' }
#'
#' @export
adjacency_matrix <- function(dataset, ..., data) {
  require(Matrix, quietly = TRUE)

  rlang::check_dots_empty()
  check_dataset_downloaded(dataset)
  data_uri <- get_dataset_cached_uri(dataset)

  mat <- readRDS(file.path(data_uri, "extra", "grid-adjacency-matrix.rds"))

  if (!missing(data)) {
    is.data.frame(data) || rlang::abort("`data` must be a data frame")
    rlang::has_name(data, "cell_id") || rlang::abort("`data` must contain a column `cell_id`")

    mat <- mat[match(data$cell_id, rownames(mat)), match(data$cell_id, colnames(mat))]

    # ensure that the order matches
    stopifnot(identical(data$cell_id, rownames(mat)))
    stopifnot(identical(data$cell_id, colnames(mat)))
  }

  mat
}
