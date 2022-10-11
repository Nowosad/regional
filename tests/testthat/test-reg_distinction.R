set.seed(32)
reg_dis = reg_distinction(vr[1:2, ], volcano)

test_that("reg_distinction works", {
  expect_equal(mean(reg_dis), 1.95, tolerance = 0.01)
})

set.seed(32)
reg_dis2 = reg_distinction(vo[c(99, 453), ], ortho, sample_size = 0.5)

test_that("reg_distinction works for 3D data", {
  expect_true(reg_dis2[2] > reg_dis2[1])
})

# reg_iso2 = reg_isolation(vo, ortho, sample_size = 500)
# reg_iso2
# reg_dis2 = reg_distinction(vo, ortho, sample_size = 50)
# plot(ortho)
# plot(reg_dis2["dis"], add = TRUE)

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
# tp_dis = reg_distinction(tp, pr)
# tp_dis2 = reg_distinction(tp, pr, dist_fun = "dtw", ndim = 1)
# tp_dis3 = reg_distinction(tp, pr, dist_fun = "dtw", ndim = 2)
# tp_dis4 = reg_distinction(tp, pr, dist_fun = "dtw", ndim = 2, normalize = TRUE)
# tp_dis5 = reg_distinction(tp, pr, dist_fun = "euclidean") #philentropy
# tp_dis6 = reg_distinction(tp, pr, dist_fun = "Euclidean") #proxy
