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
#'   reg_dis = reg_distinction(vr, volcano)
#'
#'   mean(reg_dis$dis)
#'
#'   plot(volcano)
#'   plot(vect(vr), add = TRUE)
#'   plot(volcano)
#'   plot(reg_dis["dis"], add = TRUE)
#' }
reg_distinction = function(region, raster, dist_fun = "euclidean", sample_size = 20) {
  v = terra::vect(region)
  dis = vector(mode = "numeric", length = length(v))
  # dis_j = vector(mode = "numeric", length = length(ids) - 1)
  for (i in seq_len(length(v))){
    sum_dist = 0
    n_elem = 0
    for (j in setdiff(seq_len(length(v)), i)){
      vals_i = as.matrix(terra::extract(raster, v[i])[-1])
      vals_j = as.matrix(terra::extract(raster, v[j])[-1])
      if (length(vals_i) > sample_size){
        vals_i = vals_i[sample(nrow(vals_i), size = sample_size), , drop = FALSE]
      }
      if (length(vals_j) > sample_size){
        vals_j = vals_j[sample(nrow(vals_j), size = sample_size), , drop = FALSE]
      }
      for (vi in seq_len(nrow(vals_i))){
        for (vj in seq_len(nrow(vals_j))){
          pair_of_vals = rbind(vals_i[vi, ], vals_j[vj, ])
          tmp_dist = philentropy::distance(pair_of_vals,
                                           method = dist_fun,
                                           mute.message = TRUE)
          sum_dist = sum_dist + tmp_dist
          n_elem = n_elem + 1
        }
      }
    }
    dis[i] = sum_dist / n_elem
  }
  region$dis = dis
  return(region)
}
