library(terra)
library(sf)
library(philentropy)
volcano = rast(system.file("raster/volcano.tif", package = "supercells"))
vr = read_sf(system.file("regions/volcano_regions.gpkg", package = "laland"))


# version 1 ---------------------------------------------------------------
n = 20
dist_fun = "euclidean"
v = vect(vr)
inh = vector(mode = "numeric", length = length(ids))
for (i in seq_len(nrow(vr))){
  # https://github.com/rspatial/terra/issues/275
  # terra::extract(volcano, vect(vr[i, ]), exact = FALSE)
  vals_i = terra::extract(volcano, v[i])
  if (length(vals_i) > n){
    vals_i = vals_i[sample(nrow(vals_i), size = n), , drop = FALSE]
  }
  inh[i] = mean(distance(vals_i, method = dist_fun, as.dist.obj = TRUE, mute.message = TRUE))
}
vr$inh = inh
mean(inh)

plot(volcano)
plot(vect(vr), add = TRUE)
plot(volcano)
plot(vr["inh"], add = TRUE)

# version 2 ---------------------------------------------------------------
# dist_fun = "euclidean"
# v = vect(vr)
# inh = vector(mode = "numeric", length = length(ids))
# for (i in seq_len(nrow(vr))){
#   tcm = mask(crop(volcano, v[i]), v[i])
#   spatSample(tcm, size = n)
#   vals_i = terra::extract(volcano, v[i])
#   inh[i] = mean(distance(vals_i, method = dist_fun, as.dist.obj = TRUE, mute.message = TRUE))
# }
# vr$inh = inh
# mean(inh)


