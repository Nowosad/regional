library(terra)
library(sf)
library(supercells)
volcano = rast(system.file("raster/volcano.tif", package = "supercells"))

plot(volcano)

slic = supercells(volcano, k = 100, compactness = 1)

plot(volcano)
plot(vect(slic), add = TRUE)

dir.create("inst/regions", recursive = TRUE)
write_sf(slic, "inst/regions/volcano_regions.gpkg")
