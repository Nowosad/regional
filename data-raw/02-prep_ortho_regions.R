library(terra)
library(sf)
library(supercells)
ortho = rast(system.file("raster/ortho.tif", package = "supercells"))

plot(ortho)

slic = supercells(ortho, k = 1000, compactness = 1, transform = "to_LAB")

plot(ortho)
plot(vect(slic), add = TRUE)

dir.create("inst/regions", recursive = TRUE)
write_sf(slic, "inst/regions/ortho_regions.gpkg")
