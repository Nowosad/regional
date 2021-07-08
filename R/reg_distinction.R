library(terra)
library(sf)
library(philentropy)
volcano = rast(system.file("raster/volcano.tif", package = "supercells"))
vr = read_sf(system.file("regions/volcano_regions.gpkg", package = "laland"))


# version 1 ---------------------------------------------------------------
n = 20
dist_fun = "euclidean"
v = vect(vr)
dis = vector(mode = "numeric", length = length(ids))
# dis_j = vector(mode = "numeric", length = length(ids) - 1)
for (i in seq_len(nrow(vr))){
  sum_dist = 0
  n_elem = 0
  for (j in setdiff(seq_len(nrow(vr)), i)){
    vals_i = as.matrix(terra::extract(volcano, v[i])[-1])
    vals_j = as.matrix(terra::extract(volcano, v[j])[-1])
    if (length(vals_i) > n){
      vals_i = vals_i[sample(nrow(vals_i), size = n), , drop = FALSE]
    }
    if (length(vals_j) > n){
      vals_j = vals_j[sample(nrow(vals_j), size = n), , drop = FALSE]
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

vr$dis = dis
mean(dis)

plot(volcano)
plot(vect(vr), add = TRUE)
plot(volcano)
plot(vr["dis"], add = TRUE)
