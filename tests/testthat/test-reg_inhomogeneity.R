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


