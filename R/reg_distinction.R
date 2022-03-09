#' Distinction
#'
#' Distinction is an average distance between the focus region and all of the other regions.
#' This value is between 0 and 1, where large value indicates that the values in the region
#' stands out from the other regions.
#'
#' @param region An object of class `sf` with a `POLYGON` or `MULTIPOLYGON` geometry type
#' @param raster An object of class SpatRaster (terra)
#' @param dist_fun Distance measure used. This function uses the `philentropy::distance` function in the background. Run `philentropy::getDistMethods()` to find possible distance measures.
#' @param sample_size Proportion of the cells inside of each region to be used in calculations. Value between 0 and 1.
#' It is also possible to specify an integer larger than 1, in which case the specified number of cells
#' of each region will be used in calculations.
#' @param unit a character string specifying the logarithm unit that should be used to
#' compute distances that depend on log computations.
#'
#' @return A vector with the distinction values
#' @export
#'
#' @examples
#' \dontrun{
#'   library(terra)
#'   if (requireNamespace("sf", quietly = TRUE)) {
#'     library(sf)
#'     volcano = rast(system.file("raster/volcano.tif", package = "regional"))
#'     vr = read_sf(system.file("regions/volcano_regions.gpkg", package = "regional"))
#'     vr$dis = reg_distinction(vr, volcano, sample_size = 0.5)
#'
#'     mean(vr$dis)
#'
#'     plot(volcano)
#'     plot(vect(vr), add = TRUE)
#'     plot(volcano)
#'     plot(vr["dis"], add = TRUE)
#'  }
#' }
reg_distinction = function(region, raster, dist_fun = "euclidean", sample_size = 1, unit = "log2") {
  # set.seed(32)
  v = terra::vect(region)
  dis = vector(mode = "numeric", length = length(v))
  # dis_j = vector(mode = "numeric", length = length(ids) - 1)
  for (i in seq_len(length(v))){
    sum_dist = 0
    n_elem = 0
    vals_i = as.matrix(terra::extract(raster, v[i])[-1])
    if (sample_size < 1){
      vals_i = vals_i[sample(nrow(vals_i), size = sample_size * nrow(vals_i)), , drop = FALSE]
    } else if (sample_size > 1) {
      vals_i = vals_i[sample(nrow(vals_i), size = min(c(nrow(vals_i), sample_size))), , drop = FALSE]
    }
    for (j in setdiff(seq_len(length(v)), i)){
      vals_j = as.matrix(terra::extract(raster, v[j])[-1])
      if (sample_size < 1){
        vals_j = vals_j[sample(nrow(vals_j), size = sample_size * nrow(vals_j)), , drop = FALSE]
      } else if (sample_size > 1) {
        vals_j = vals_j[sample(nrow(vals_j), size = min(c(nrow(vals_j), sample_size))), , drop = FALSE]
      }
      dist_mat = philentropy::dist_many_many(vals_i, vals_j, method = dist_fun, testNA = FALSE, unit = unit)
      sum_dist = sum_dist + sum(dist_mat)
      n_elem = n_elem + length(dist_mat)
    }
    dis[i] = sum_dist / n_elem
  }
  # region$dis = dis
  return(dis)
}

# reg_distinction0 = function(region, raster, dist_fun = "euclidean", sample_size = 20) {
#   set.seed(32)
#   v = terra::vect(region)
#   dis = vector(mode = "numeric", length = length(v))
#   # dis_j = vector(mode = "numeric", length = length(ids) - 1)
#   for (i in seq_len(length(v))){
#     sum_dist = 0
#     n_elem = 0
#     vals_i = as.matrix(terra::extract(raster, v[i])[-1])
#     if (nrow(vals_i) > sample_size){
#       vals_i = vals_i[sample(nrow(vals_i), size = sample_size), , drop = FALSE]
#     }
#     for (j in setdiff(seq_len(length(v)), i)){
#       vals_j = as.matrix(terra::extract(raster, v[j])[-1])
#       if (nrow(vals_j) > sample_size){
#         vals_j = vals_j[sample(nrow(vals_j), size = sample_size), , drop = FALSE]
#       }
#       for (vi in seq_len(nrow(vals_i))){
#         for (vj in seq_len(nrow(vals_j))){
#           pair_of_vals = rbind(vals_i[vi, ], vals_j[vj, ])
#           tmp_dist = philentropy::distance(pair_of_vals,
#                                            method = dist_fun,
#                                            mute.message = TRUE)
#           sum_dist = sum_dist + tmp_dist
#           n_elem = n_elem + 1
#         }
#       }
#     }
#     dis[i] = sum_dist / n_elem
#   }
#   region$dis = dis
#   return(region)
# }
# reg_distinction3 = function(region, raster, dist_fun = "euclidean", sample_size = 20) {
#   set.seed(32)
#   v = terra::vect(region)
#   dis = vector(mode = "numeric", length = length(v))
#   # dis_j = vector(mode = "numeric", length = length(ids) - 1)
#   for (i in seq_len(length(v))){
#     vals_i = as.matrix(terra::extract(raster, v[i])[-1])
#     if (nrow(vals_i) > sample_size){
#       vals_i = vals_i[sample(nrow(vals_i), size = sample_size), , drop = FALSE]
#     }
#     v_sample = spatSample(v[-i], size = sample_size, method = "random")
#     vals_j = as.matrix(terra::extract(raster, v_sample)[-1])
#     dist_mat = philentropy:::dist_many_many(vals_i, vals_j, dist_fun = dist_fun, testNA = FALSE, unit = "log2")
#     dis[i] = mean(dist_mat)
#   }
#   region$dis = dis
#   return(region)
# }


#
#
# library(terra)
# library(sf)
# volcano = rast(system.file("raster/volcano.tif", package = "supercells"))
# vr = read_sf(system.file("regions/volcano_regions.gpkg", package = "regional"))
# reg_inh = reg_distinction(vr, volcano)
#
# bench::mark(
#   # reg_distinction(vr, volcano),
#             reg_distinction2(vr, volcano),
#             reg_distinction3(vr, volcano, sample_size = 400), check = FALSE)
#
# profvis::profvis(reg_distinction(vr, volcano))
# profvis::profvis(reg_distinction2(vr, volcano))
#
# rd1 = reg_distinction2(vr, volcano, sample_size = 30)
# rd2 = reg_distinction3(vr, volcano, sample_size = 400)
# plot(rd1$dis, rd2$dis)
# cor(rd1$dis, rd2$dis)
