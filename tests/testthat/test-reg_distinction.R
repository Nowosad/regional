set.seed(32)
reg_dis = reg_distinction(vr[1:2, ], volcano)

test_that("reg_distinction works", {
  expect_equal(mean(reg_dis$dis), 1.975)
})
