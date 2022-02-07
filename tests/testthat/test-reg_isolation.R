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
