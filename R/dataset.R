#' Human Ecology Grid dataset descriptor
#'
#' @param resolution grid resolution in kilometers
#' @param download whether to download the dataset if it is not cached
#'
#' This function creates a descriptor for a specific version and grid resolution of the Human Ecology Grid Database.
#' The descriptor can be used to access the data and metadata.
#'
#' @examples
#' \dontrun{
#' data_descriptor <- humanEcologyGrid::dataset(resolution = 300, download = TRUE)
#' humanEcologyGrid::latest_data(data_descriptor)
#' }
#'
#' @export
dataset <- function(..., resolution, download = FALSE) {
  rlang::check_dots_empty()
  resolution <- check_grid_resolution(resolution)

  dataset <- structure(list(
    resolution = resolution
  ), class = "human-ecology-grid-dataset")
  check_dataset_downloaded(dataset, download = download)
  dataset
}

#' @export
`print.human-ecology-grid-dataset` <- function(x, ...) {
  .arg <- substitute(x)
  .arg <- if (is.name(.arg)) as.character(.arg) else "dataset"

  cat(sprintf("# Human Ecology Grid Data at %skm grid resolution\n", x$resolution))
  writeLines(rlang::format_error_bullets(c(
    i = sprintf("use `humanEcologyGrid::yerly_data(%s)` to get the longitudinal (yearly) data", .arg),
    i = sprintf("use `humanEcologyGrid::latest_data(%s)` to get the latest cross-sectional data", .arg),
    i = sprintf("use `humanEcologyGrid::grid_cells(%s)` to get the grid cells geometry", .arg),
    i = sprintf("use `humanEcologyGrid::grid_adjacency(%s)` to get the adjacency matrix", .arg)
  )))
}

get_dataset_cached_uri <- function(dataset, check = FALSE) {
  data_root <- system.file(package = "humanEcologyGrid")
  if (data_root == "") data_root <- getwd()
  data_uri <- file.path(data_root, "downloaded_data", sprintf("data-%skm", dataset$resolution))

  data_uri
}

check_dataset_downloaded <- function(dataset, download = FALSE) {
  data_uri <- get_dataset_cached_uri(dataset)
  if(dir.exists(data_uri)) return(TRUE)

  if (!isTRUE(download) && interactive() ) {
    prompt <- sprintf("Human Ecology Grid data at %skm resolution is not cached, download?", dataset$resolution)
    download <- askYesNo(prompt)

    isTRUE(download) || rlang::abort("Aborting due to user choice")
  }

  if (isTRUE(download)) {
    download_dataset(dataset)
    return(TRUE)
  }

  rlang::abort(sprintf("Missing data for Human Ecology Grid data at %skm resolution", dataset$resolution))
}

download_dataset <- function(dataset) {
  local_uri <- get_dataset_cached_uri(dataset)
  resolution <- dataset$resolution

  # check if the local uri exists
  if (dir.exists(local_uri)) return(TRUE)

  # check if we have access to the location
  check_errors(dir.create(local_uri, showWarnings = FALSE, recursive = TRUE))
  if (!dir.exists(local_uri)) rlang::abort(sprintf("unable to create download directory '%s'", local_uri))


  # prepare to download
  success <- FALSE
  zipfile <- sprintf("human-ecology-grid-%skm.zip", resolution)
  tmpdir <- tempdir(check = TRUE)
  tmpfile <- file.path(tmpdir, zipfile)

  # clean up action
  on.exit({
    # remove the data folder if the download was not successful
    if (!success) check_errors(unlink(local_uri, force = TRUE, recursive = TRUE))
    check_errors(unlink(tmpfile, force = TRUE))
  })

  # download the asset
  remote_uri <- paste0("https://github.com/human-ecology-grid/human-ecology-grid/releases/download/1.0-rc/", zipfile)
  utils::download.file(remote_uri, tmpfile)
  utils::unzip(tmpfile, exdir = local_uri)
  success <- TRUE

  cat(sprintf("i Successfully downloaded %skm resolution data\n", resolution))
}


check_grid_resolution <- function(resolution, ..., .arg = rlang::caller_arg(resolution)) {
  rlang::check_dots_empty()
  known_resolutions <- c(50L, 100L, 150L, 300L, 500L)

  !missing(resolution) || rlang::abort(c(
    sprintf("missing required argument `%s%`", .arg),
    "i" = "available grid resolutions are",
    rlang::set_names(paste0(known_resolutions, "km"), rlang::rep_along(known_resolutions, "-"))
  ))

  resolution0 <- if (rlang::is_integerish(resolution, 1L, finite = TRUE) && resolution > 0L) {
    as.integer(resolution)
  } else if (rlang::is_string(resolution)) {
    resolution0 <- gsub("km$", "", resolution, ignore.case = TRUE)
    suppressWarnings(as.integer(resolution0))
  } else {
    resolution
  }

  rlang::is_integer(resolution0, 1L) && (resolution0 %in% known_resolutions) || rlang::abort(c(
    sprintf("invalid grid resolution `%s = %s`", .arg, rlang::as_label(resolution)),
    "i" = "resolution must be a grid resolution in km (either as string or a number)",
    "i" = "available grid resolutions are",
    rlang::set_names(paste0(known_resolutions, "km"), rlang::rep_along(known_resolutions, "-"))
  ))

  resolution0
}
