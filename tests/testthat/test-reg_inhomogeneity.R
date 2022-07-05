set.seed(32)
reg_inh = reg_inhomogeneity(vr[1:2, ], volcano)

test_that("reg_inhomogeneity works", {
  expect_equal(mean(reg_inh), 0.2527044, tolerance = 0.00001)
})

set.seed(32)
reg_inh2 = reg_inhomogeneity(vo[c(99, 453), ], ortho, sample_size = 0.5)

test_that("reg_inhomogeneity works for 3D data", {
  expect_true(reg_inh2[1] > reg_inh2[2])
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
# tp_inh = reg_inhomogeneity(tp, pr)
# tp_inh2 = reg_inhomogeneity(tp, pr, dist_fun = "dtw", ndim = 1)
# tp_inh3 = reg_inhomogeneity(tp, pr, dist_fun = "dtw", ndim = 2)
# tp_inh4 = reg_inhomogeneity(tp, pr, dist_fun = "dtw", ndim = 2, normalize = TRUE)
# tp_inh5 = reg_inhomogeneity(tp, pr, dist_fun = "euclidean") #philentropy
# tp_inh6 = reg_inhomogeneity(tp, pr, dist_fun = "Euclidean") #proxy
