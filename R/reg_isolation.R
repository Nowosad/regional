#' Isolation
#'
#' Isolation is an average distance between the focus region
#' and all of its neighbors. This value is between 0 and 1,
#' where large value indicates that values of the region
#' stands out from its surroundings.
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
#' @return A vector with the isolation values
#' @export
#'
#' @examples
#' \dontrun{
#'  library(terra)
#'  library(sf)
#'  volcano = rast(system.file("raster/volcano.tif", package = "supercells"))
#'  vr = read_sf(system.file("regions/volcano_regions.gpkg", package = "regional"))
#'  vr$iso = reg_isolation(vr, volcano, sample_size = 1)
#'
#'  mean(volcano$iso)
#'
#'  plot(volcano)
#'  plot(vect(vr), add = TRUE)
#'  plot(volcano)
#'  plot(vr["iso"], add = TRUE)
#'}
reg_isolation = function(region, raster, dist_fun = "euclidean", sample_size = 1, unit = "log2") {
  # set.seed(32)
  v = terra::vect(region)
  iso = vector(mode = "numeric", length = length(v))
  for (i in seq_len(length(v))){
    sum_dist = 0
    n_elem = 0
    vals_i = as.matrix(terra::extract(raster, v[i])[-1])
    if (sample_size < 1){
      vals_i = vals_i[sample(nrow(vals_i), size = sample_size * nrow(vals_i)), , drop = FALSE]
    } else if (sample_size > 1) {
      vals_i = vals_i[sample(nrow(vals_i), size = min(c(nrow(vals_i), sample_size))), , drop = FALSE]
    }
    neigh_id = which(terra::relate(v, v[i], relation = "touches"))
    for (j in neigh_id){
      vals_j = as.matrix(terra::extract(raster, v[j])[-1])
      if (sample_size < 1){
        vals_j = vals_j[sample(nrow(vals_j), size = sample_size * nrow(vals_j)), , drop = FALSE]
      } else if (sample_size > 1) {
        vals_j = vals_j[sample(nrow(vals_j), size = min(c(nrow(vals_j), sample_size))), , drop = FALSE]
      }
      dist_mat = philentropy::dist_many_many(vals_i, vals_j,
                                             method = dist_fun,
                                             testNA = FALSE, unit = unit)
      sum_dist = sum_dist + sum(dist_mat)
      n_elem = n_elem + length(dist_mat)
      # cat(" j:", j)
    }
    # cat("\n", "i:", i)
    iso[i] = sum_dist/n_elem
    # iso[i] = ifelse(sum_dist == 0 & n_elem == 0, 0, sum_dist/n_elem)
  }
  # region$iso = iso
  return(iso)
}

# #' @export
# reg_isolation0 = function(region, raster, dist_fun = "euclidean", sample_size = 20) {
#   set.seed(32)
#   v = terra::vect(region)
#   iso = vector(mode = "numeric", length = length(v))
#   for (i in seq_len(length(v))){
#     sum_dist = 0
#     n_elem = 0
#     vals_i = as.matrix(terra::extract(raster, v[i])[-1])
#     if (nrow(vals_i) > sample_size){
#       vals_i = vals_i[sample(nrow(vals_i), size = sample_size), , drop = FALSE]
#     }
#     neigh_id = which(terra::relate(v, v[i], relation = "touches"))
#     for (j in neigh_id){
#       vals_j = as.matrix(terra::extract(raster, v[j])[-1])
#       if (nrow(vals_j) > sample_size){
#         vals_j = vals_j[sample(nrow(vals_j), size = sample_size), , drop = FALSE]
#       }
#       for (vi in seq_len(nrow(vals_i))){
#         for (vj in seq_len(nrow(vals_j))){
#           # cat("i = ", i, "; j = ", j,  "; vi = ", vi,  "; vj = ", vj)
#           # i=  42 ; j =  24 ; vi =  1 ; vj =  20
#           # Error in vals_i[vj, ] : subscript out of bounds
#           # pair_of_vals = rbind(vals_i[vi, ], vals_j[vj, ])
#           tmp_dist = philentropy:::single_distance(vals_i[vi, ],
#                                                    vals_j[vj, ],
#                                                    dist_fun = dist_fun,
#                                                    FALSE, "")
#           sum_dist = sum_dist + tmp_dist
#           n_elem = n_elem + 1
#         }
#       }
#     }
#     iso[i] = sum_dist / n_elem
#   }
#   region$iso = iso
#   return(region)
# }
# #' @export
# reg_isolation3 = function(region, raster, dist_fun = "euclidean", sample_size = 50) {
#   # set.seed(32)
#   v = terra::vect(region)
#   iso = vector(mode = "numeric", length = length(v))
#   for (i in seq_len(length(v))){
#     vals_i = as.matrix(terra::extract(raster, v[i])[-1])
#     if (nrow(vals_i) > sample_size){
#       vals_i = vals_i[sample(nrow(vals_i), size = sample_size), , drop = FALSE]
#     }
#     neigh_id = which(terra::relate(v, v[i], relation = "touches"))
#     neigh_v = v[neigh_id]
#     neigh_v_sample = spatSample(neigh_v, size = sample_size, method = "random")
#     vals_j = as.matrix(terra::extract(raster, neigh_v_sample)[-1])
#     dist_mat = philentropy:::dist_many_many(vals_i, vals_j,
#                                             dist_fun = dist_fun,
#                                             FALSE, "")
#     iso[i] = mean(dist_mat)
#   }
#   region$iso = iso
#   return(region)
# }


# library(terra)
# library(sf)
# volcano = rast(system.file("raster/volcano.tif", package = "supercells"))
# vr = read_sf(system.file("regions/volcano_regions.gpkg", package = "regional"))
# reg_inh = reg_isolation2(vr, volcano)
#
# bench::mark(reg_isolation(vr, volcano),
#             reg_isolation2(vr, volcano))
# gc()
#
# profvis::profvis(reg_isolation(vr, volcano))
# profvis::profvis(reg_isolation2(vr, volcano))
# profvis::profvis(reg_isolation3(vr, volcano))
#
#
# i1 = reg_isolation(vr, volcano)
# profvis::profvis({i2 = reg_isolation2(vr, volcano, sample_size = 100)})
# profvis::profvis({i3 = reg_isolation3(vr, volcano, sample_size = 150)})
#
# bench::mark(reg_isolation(vr, volcano, sample_size = 100),
#             reg_isolation2(vr, volcano, sample_size = 100),
#             reg_isolation3(vr, volcano, sample_size = 150),
#             check = FALSE)
