#' Inhomogeneity
#'
#' Inhomogeneity measures a degree of mutual dissimilarity
#' between values of all cells in a region. This value is between 0 and 1,
#' where small value indicates that values of all cells in the region
#' represent consistent patterns so the cluster is pattern-homogeneous.
#'
#' @param region An object of class `sf` with a `POLYGON` or `MULTIPOLYGON` geometry type
#' @param raster An object of class SpatRaster (terra)
#' @param dist_fun Distance measure used. This function uses `philentropy::distance` (run `philentropy::getDistMethods()` to find possible distance measures) or `proxy::dist` (run `names(proxy::pr_DB$get_entries())` to find possible distance measures) in the background.
#' It is also possible to use `"dtw"` (dynamic time warping)
#' @param sample_size Proportion of the cells inside of each region to be used in calculations. Value between 0 and 1.
#' It is also possible to specify an integer larger than 1, in which case the specified number of cells
#' of each region will be used in calculations.
#' @param unit A character string specifying the logarithm unit that should be used to
#' compute distances that depend on log computations.
#' @param na.rm Whether NA values should be stripped from the calculations.
#' @param ... Additional arguments for `philentropy::dist_one_one`, `proxy::dist`, or `dtwclust::dtw_basic`.
#' When `dist_fun = "dtw"` is used, `ndim` should be set to specify how many dimension the input raster time-series has.
#'
#' @return A vector with the inhomogeneity values
#' @export
#'
#' @examples
#' \dontrun{
#'   library(terra)
#'   if (requireNamespace("sf", quietly = TRUE)) {
#'     library(sf)
#'     volcano = rast(system.file("raster/volcano.tif", package = "regional"))
#'     vr = read_sf(system.file("regions/volcano_regions.gpkg", package = "regional"))
#'     vr$inh = reg_inhomogeneity(vr, volcano, sample_size = 1)
#'
#'     mean(vr$inh)
#'
#'     plot(volcano)
#'     plot(vect(vr), add = TRUE)
#'     plot(volcano)
#'     plot(vr["inh"], add = TRUE)
#'  }
#' }
reg_inhomogeneity = function(region, raster, dist_fun = "euclidean", sample_size = 1, unit = "log2", na.rm = FALSE, ...) {
  v = terra::vect(region)
  inh = vector(mode = "numeric", length = length(v))
  for (i in seq_len(length(v))){
    # https://github.com/rspatial/terra/issues/275
    # terra::extract(volcano, vect(vr[i, ]), exact = FALSE)
    vals_i = terra::extract(raster, v[i], ID = FALSE, raw = TRUE)
    if (sample_size < 1){
      vals_i = vals_i[sample(nrow(vals_i), size = max(sample_size * nrow(vals_i), 3), replace = TRUE), , drop = FALSE]
    } else if (sample_size > 1) {
      vals_i = vals_i[sample(nrow(vals_i), size = min(c(nrow(vals_i), sample_size))), , drop = FALSE]
    }
    inh[i] = mean(universal_distance(vals_i, dist_fun = dist_fun, ...), na.rm = na.rm)
  }
  # region$inh = inh
  return(inh)
}


# reg_inh = reg_inhomogeneity(vr, volcano, sample_size = 1)
# reg_inh2 = reg_inhomogeneity(vr, volcano, sample_size = 0.1)
#
# plot(reg_inh$inh, reg_inh2$inh)
# cor(reg_inh$inh, reg_inh2$inh)
#
# plot(volcano)
# plot(reg_inh[4, 5], add = TRUE)
