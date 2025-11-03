#' Obtain the Human Ecology Grid Cell Polygons
#'
#' @param dataset         the dataset descriptor (see `[dataset()])
#' @param split_meridian  the meridian to split the cell geometry at (see details)
#' @param as              the format to return the grid cells as (see details)
#' @param ...             reserved for future extensions
#'
#' The polygons can be optionally split at a given meridian for plotting or other applications
#' using tools that do not support spherical geometry. The default option is to split at the
#' antimeridian (avoids artifacts when plotting using the default projection).
#'
#' The function returns a simple features (`sf`) object by default. To return a tibble with
#' `s2` polygons set `as = "s2"`.
#'
#' @examples
#' \dontrun{
#' data_descriptor <- humanEcologyGrid::dataset(resolution = 300, download = TRUE)
#' # cells for Pacific-Centered maps
#' humanEcologyGrid::grid_cells(data_descriptor, split_meridian = -30)
#' }
#'
#' @export
grid_cells <- function(dataset, ..., split_meridian = 180, as = c("sf", "s2")) {
  rlang::check_dots_empty()
  check_dataset_downloaded(dataset)
  data_uri <- get_dataset_cached_uri(dataset)

  requireNamespace("sf", quietly = TRUE)

  is.null(split_meridian) ||
  (rlang::is_bare_numeric(split_meridian, 1L) && (abs(split_meridian <= 180))) || rlang::abort(c(
    "`split_meridian` should be a longitude between -180.0 and 180.0",
    i = sprintf("got `%s`", rlang::as_label(split_meridian))
  ))

  is.null(split_meridian) || rlang::is_installed("s2") || rlang::abort(
    "package `s2` is required to split by meridian"
  )

  as <- rlang::arg_match(as)

  # read the geojson
  geojson_path <- file.path(data_uri, "grid.geojson.xz")
  cells <- tryCatch({

    text <- if (rlang::is_installed("readr")) {
      readr::read_file(geojson_path)
    } else {
      readLines(geojson_path, warn = FALSE, encoding = "UTF-8")
    }

    # parse the file
    opts <- yyjsonr::opts_read_geojson(property_promotion = "list")
    yyjsonr::read_geojson_str(text, opts = opts)
  }, error = function(cnd) {
   rlang::abort(c(
     sprintf("unable to load GeoJSON at \"%s\"", geojson_path),
     i = cnd$message
   ))
  })

  # split the meridian
  if (!is.null(split_meridian)) {
    # build a thin polygon representing the split meridian
    tol <- 0.01
    meridian_geometry <- s2::s2_make_polygon(
      c(split_meridian + tol, split_meridian - tol, split_meridian - tol, split_meridian + tol),
      c(90.0, 90.0, -90.0, -90.0),
      oriented = TRUE
    )
    opts <- s2::s2_options(dimensions = "polygon")

    cell_geometry <- s2::as_s2_geography(cells$geometry)
    cells$geometry <- sf::st_as_sfc(s2::s2_difference(cell_geometry, meridian_geometry, options = opts))
  }

  if (identical(as, "s2")) {
    cells <- tibble::as_tibble(cells)
    cells$geometry <- s2::as_s2_geography(cells$geometry)
  }

  cells
}
