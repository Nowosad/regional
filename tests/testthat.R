library(testthat)
library(laland)
library(terra)
library(sf)
volcano = rast(system.file("raster/volcano.tif", package = "supercells"))
vr = read_sf(system.file("regions/volcano_regions.gpkg", package = "laland"))

test_check("laland")
