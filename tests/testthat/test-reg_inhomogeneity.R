reg_inh = reg_inhomogeneity(vr[1:2, ], volcano)

test_that("reg_inhomogeneity works", {
  expect_equal(mean(reg_inh$inh), 0.2527044, tolerance = 0.00001)
})
