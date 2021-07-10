set.seed(432)
reg_iso = reg_isolation(vr[1:2, ], volcano)

test_that("reg_isolation works", {
  expect_equal(mean(reg_iso$iso), 2)
})
