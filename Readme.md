# humanEcologyGrid package
Access to the Human Ecology Grid Database in R


## Installation

```r
# using pak
pak::pak("human-ecology-grid/R-package-humanEcologyGrid")
# using remotes
remotes::install_github("human-ecology-grid/R-package-humanEcologyGrid")
```

## Usage

> [!NOTE]
> Please avoid loading the package with `library(humanEcologyGrid)`. For readability and to avoid the
> naming conflicts all functions in the package are intended to be called using the package prefix, e.g.
> `humanEcologyGrid::dataset(grid_resolution = "150km")`


Create a dataset descriptor for a given grid resolution and database version

```r
data_descriptor <- humanEcologyGrid::dataset(resolution = "150km")
```

Load the gridded panel data

```r
# panel data (all years) as Arrow Dataset
humanEcologyGrid::yearly_data(data_descriptor, arrow = TRUE)
# panel data (all years) as tibble (not recommeded for higher resolutions due to data size)
humanEcologyGrid::yearly_data(data_descriptor, arrow = FALSE)
# panel data (selected year) as tibble
humanEcologyGrid::yearly_data(data_descriptor, year = 2000, arrow = FALSE)
```

Load the gridded cross-sectional data (most recent available annual data for each data module)

```r
# cross-sectional data as Arrow Dataset
humanEcologyGrid::latest_data(data_descriptor, arrow = TRUE)
# cross-sectional data as tibble
humanEcologyGrid::latest_data(data_descriptor, arrow = FALSE)
```


Load the grid cells geometry

```r
cells <- humanEcologyGrid::grid_cells(data_descriptor)
ggplot(cells)  + geom_sf(aes(fill = cell_is_ocean_ice))

# Pacific-Centered map pre-split on -30 meridian  using Equal Earth projection
pacific_cells <- humanEcologyGrid::grid_cells(data_descriptor, split_meridian = -30)
ggplot(cells)  + geom_sf(aes(fill = cell_is_ocean_ice)) + coord_sf(crs = 8859)
```

Load the adjacency matrix (e.g. to perform spatial regression)

```r
rows <- humanEcologyGrid::latest_data(data_descriptor, arrow = FALSE) |>
  select(cell_id, languages_num, languages_endangered_num) |>
  drop_na()

# adjacency matrix filtered and matched to data
W <- humanEcologyGrid::adjacency_matrix(data_descriptor, data = rows)
```
