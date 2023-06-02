library(terra)
library(sf)
volcano = rast(system.file("raster/volcano.tif", package = "regional"))
vr = read_sf(system.file("regions/volcano_regions.gpkg", package = "regional"))
profvis::profvis(reg_isolation(vr, volcano, sample_size = 0.01))
mean(vr$iso)
plot(volcano)
plot(vect(vr), add = TRUE)
plot(volcano)
plot(vr["iso"], add = TRUE)

library(exactextractr)
a = as.matrix(exact_extract(volcano, vr[i, ])[[1]][, 1])
a

vr2 = as.data.frame(vect(vr))

vrv = vect(vr)
cells(volcano, vrv)

volcano2 = vrt(system.file("raster/volcano.tif", package = "regional"))

devtools::load_all()
profvis::profvis(reg_isolation(vr, volcano, sample_size = 1))
profvis::profvis(reg_isolation(vr, volcano2, sample_size = 1))


m = matrix(runif(999999990), ncol = 2)
dim(m)
format(object.size(m), "MB")

profvis::profvis(extract(volcano, vr[1, ]))

devtools::load_all()
profvis::profvis(reg_inhomogeneity(vr, volcano, sample_size = 1))
profvis::profvis(reg_inhomogeneity2(vr, volcano2, sample_size = 1))

volcano2 = disagg(volcano, 20)
ncell(volcano2)

bench::mark(reg_inhomogeneity(vr, volcano2, sample_size = 1),
            reg_inhomogeneity2(vr, volcano2, sample_size = 1),
            reg_inhomogeneity3(vr, volcano2, sample_size = 1),
            reg_inhomogeneity4(vr, volcano2, sample_size = 1)
            )

bench::mark(reg_inhomogeneity(vr, volcano, sample_size = 500),
            reg_inhomogeneity2(vr, volcano, sample_size = 500),
            reg_inhomogeneity3(vr, volcano, sample_size = 500),
            reg_inhomogeneity4(vr, volcano, sample_size = 500),
            reg_inhomogeneity5(vr, volcano, sample_size = 500),
            iterations = 5
            )

library(regional)
bench::mark(reg_inhomogeneity(vr, volcano, sample_size = 500),
            reg_inhomogeneity4(vr, volcano, sample_size = 500),
            reg_inhomogeneity5(vr, volcano, sample_size = 500),
            iterations = 5
)

profvis::profvis(reg_inhomogeneity5(vr, volcano, sample_size = 500))
profvis::profvis(reg_inhomogeneity4(vr, volcano, sample_size = 500))
