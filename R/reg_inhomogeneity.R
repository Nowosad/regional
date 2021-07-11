#' Title
#'
#' @param region
#' @param raster
#' @param dist_fun
#' @param sample_size
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#'   library(terra)
#'   library(sf)
#'   volcano = rast(system.file("raster/volcano.tif", package = "supercells"))
#'   vr = read_sf(system.file("regions/volcano_regions.gpkg", package = "laland"))
#'   reg_inh = reg_inhomogeneity(vr, volcano)
#'
#'   mean(reg_inh$inh)
#'
#'   plot(volcano)
#'   plot(vect(vr), add = TRUE)
#'   plot(volcano)
#'   plot(reg_inh["inh"], add = TRUE)
#' }
reg_inhomogeneity = function(region, raster, dist_fun = "euclidean", sample_size = 20) {
  v = terra::vect(region)
  inh = vector(mode = "numeric", length = length(v))
  for (i in seq_len(length(v))){
    # https://github.com/rspatial/terra/issues/275
    # terra::extract(volcano, vect(vr[i, ]), exact = FALSE)
    vals_i = terra::extract(raster, v[i])
    if (nrow(vals_i) > sample_size){
      vals_i = vals_i[sample(nrow(vals_i), size = sample_size), , drop = FALSE]
    }
    inh[i] = mean(philentropy::distance(vals_i, method = dist_fun,
                                        as.dist.obj = TRUE, mute.message = TRUE))
  }
  region$inh = inh
  return(region)
}
