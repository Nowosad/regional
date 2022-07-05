set.seed(432)
reg_iso = reg_isolation(vr[1:2, ], volcano)

test_that("reg_isolation works", {
  expect_equal(mean(reg_iso), 1.95, tolerance = 0.01)
})

set.seed(32)
reg_iso2 = reg_isolation(vo[c(304, 295, 308, 326, 331, 285), ], ortho, sample_size = 0.5)

test_that("reg_isolation works for 3D data", {
  expect_true(all(reg_iso2[5] > reg_iso2[4]))
})

# library(terra)
# library(tmap)
# ta = rast(system.file("raster/ta_scaled.tif", package = "spquery")) |>
#   aggregate(fact = 8, fun = "median", na.rm = TRUE)
# pr = rast(system.file("raster/pr_scaled.tif", package = "spquery")) |>
#   aggregate(fact = 8, fun = "median", na.rm = TRUE)
# #
# # plot(pr, range = c(0, 1))
# tp = supercells::supercells(ta, k = 7, compactness = 0.4)
# tm_shape(pr) + tm_raster(col.legend = tm_legend(show = FALSE)) +
#   tm_shape(tp) + tm_borders(lwd = 4, col = "black")
#
# set.seed(32)
# tp_iso = reg_isolation(tp, pr)
# tp_iso2 = reg_isolation(tp, pr, dist_fun = "dtw", ndim = 1)
# tp_iso3 = reg_isolation(tp, pr, dist_fun = "dtw", ndim = 2)
# tp_iso4 = reg_isolation(tp, pr, dist_fun = "dtw", ndim = 2, normalize = TRUE)
# tp_iso5 = reg_isolation(tp, pr, dist_fun = "euclidean") #philentropy
# tp_iso6 = reg_isolation(tp, pr, dist_fun = "Euclidean") #proxy
