#' Inhomogeneity
#'
#' Inhomogeneity measures a degree of mutual dissimilarity
#' between values of all cells in a region. This value is between 0 and 1,
#' where small value indicates that values of all cells in the region
#' represent consistent patterns so the cluster is pattern-homogeneous.
#'
#' @param region An object of class `sf` with a `POLYGON` or `MULTIPOLYGON` geometry type
#' @param raster An object of class SpatRaster (terra)
#' @param dist_fun Distance measure used. This function uses the `philentropy::distance` function in the background. Run `philentropy::getDistMethods()` to find possible distance measures.
#' @param sample_size Proportion of the cells inside of each region to be used in calculations. Value between 0 and 1.
#'
#' @return A vector with the inhomogeneity values
#' @export
#'
#' @examples
#' \dontrun{
#'   library(terra)
#'   library(sf)
#'   volcano = rast(system.file("raster/volcano.tif", package = "supercells"))
#'   vr = read_sf(system.file("regions/volcano_regions.gpkg", package = "regional"))
#'   reg_inh = reg_inhomogeneity(vr, volcano, sample_size = 0.5)
#'
#'   mean(reg_inh$inh)
#'
#'   plot(volcano)
#'   plot(vect(vr), add = TRUE)
#'   plot(volcano)
#'   plot(reg_inh["inh"], add = TRUE)
#' }
reg_inhomogeneity = function(region, raster, dist_fun = "euclidean", sample_size = 1) {
  v = terra::vect(region)
  inh = vector(mode = "numeric", length = length(v))
  for (i in seq_len(length(v))){
    # https://github.com/rspatial/terra/issues/275
    # terra::extract(volcano, vect(vr[i, ]), exact = FALSE)
    vals_i = as.matrix(terra::extract(raster, v[i])[-1])
    if (sample_size < 1){
      vals_i = vals_i[sample(nrow(vals_i), size = max(sample_size * nrow(vals_i), 3), replace = TRUE), , drop = FALSE]
    }
    inh[i] = mean(philentropy::distance(vals_i, method = dist_fun,
                                        as.dist.obj = TRUE, mute.message = TRUE))
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
