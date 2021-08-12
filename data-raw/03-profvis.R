profvis::profvis({
  library(terra)
  library(sf)
  volcano = rast(system.file("raster/volcano.tif", package = "supercells"))
  vr = read_sf(system.file("regions/volcano_regions.gpkg", package = "regional"))
  reg_inh = reg_inhomogeneity(vr, volcano)
  reg_inhomogeneity = function(region, raster, dist_fun = "euclidean", sample_size = 20) {
    v = terra::vect(region)
    inh = vector(mode = "numeric", length = length(v))
    for (i in seq_len(length(v))){
      # https://github.com/rspatial/terra/issues/275
      # terra::extract(volcano, vect(vr[i, ]), exact = FALSE)
      vals_i = terra::extract(raster, v[i])
      if (length(vals_i) > sample_size){
        vals_i = vals_i[sample(nrow(vals_i), size = sample_size), , drop = FALSE]
      }
      inh[i] = mean(philentropy::distance(vals_i, method = dist_fun,
                                          as.dist.obj = TRUE, mute.message = TRUE))
    }
    region$inh = inh
    return(region)
  }
})


profvis::profvis({
  library(terra)
  library(sf)
  volcano = rast(system.file("raster/volcano.tif", package = "supercells"))
  vr = read_sf(system.file("regions/volcano_regions.gpkg", package = "regional"))
  reg_inh = reg_isolation(vr, volcano)

  reg_isolation = function(region, raster, dist_fun = "euclidean", sample_size = 20) {
    v = terra::vect(region)
    iso = vector(mode = "numeric", length = length(v))
    for (i in seq_len(length(v))){
      sum_dist = 0
      n_elem = 0
      neigh_id = which(terra::relate(v, v[i], relation = "touches"))
      for (j in neigh_id){
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
                                             # as.dist.obj = TRUE,
                                             mute.message = TRUE)
            sum_dist = sum_dist + tmp_dist
            n_elem = n_elem + 1
          }
        }
      }
      iso[i] = sum_dist / n_elem
    }
    region$iso = iso
    return(region)
  }
})
