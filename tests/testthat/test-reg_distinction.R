set.seed(32)
reg_dis = reg_distinction(vr[1:2, ], volcano)

test_that("reg_distinction works", {
  expect_equal(mean(reg_dis$dis), 1.975)
})

set.seed(32)
reg_inh2 = reg_distinction(vo[c(99, 453), ], ortho, sample_size = 50)
reg_inh2

test_that("reg_isolation works for 3D data", {
  expect_true(reg_inh2$inh[1] > reg_inh2$inh[2])
})

reg_inh2 = reg_isolation(vo, ortho, sample_size = 500)
reg_inh2


reg_dis2 = reg_distinction(vo, ortho, sample_size = 50)
plot(ortho)
plot(reg_dis2["dis"], add = TRUE)
