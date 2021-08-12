library(testthat)
library(regional)
library(terra)
library(sf)
volcano = rast(system.file("raster/volcano.tif", package = "supercells"))
vr = read_sf(system.file("regions/volcano_regions.gpkg", package = "regional"))

ortho = rast(system.file("raster/ortho.tif", package = "supercells"))
vo = supercells::supercells(ortho, k = 1000, compactness = 1)

test_check("regional")
