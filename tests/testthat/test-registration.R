library(refund)
library(registr)

context("registration plots")


test_that("no errors arise for binary registration plots",{
  skip_on_travis()
  registration_data = simulate_unregistered_curves(I = 20, D = 50, seed = 2018)
  binary_registration = register_fpca(Y = registration_data, family = "binomial",
                                      Kt = 6, Kh = 4, npc  = 1)
  expect_error(plot_shiny(binary_registration), NA)

  #plot_shiny(binary_registration, thin_data = TRUE)

})

test_that("no errors arise for gaussian registration plots",{
  skip_on_travis()
  registration_data = simulate_unregistered_curves(I = 20, D = 50, seed = 2018)
  registration_data$value = registration_data$latent_mean
  gaussian_registration = register_fpca(Y = registration_data, family = "gaussian", npc = 2)

  expect_error(plot_shiny(gaussian_registration), NA)

})

test_that("registration works with DTI data",{
  skip_on_travis()
  data(DTI)
  DTI = filter(as_refundObj(DTI$cca), id %in% c("1001_1", "1002_1", "1003_1", "1004_1", "1005_1"))
  fit_reg = register_fpca(Y = DTI, family = "gaussian", npc = 3)
  plot_shiny(fit_reg)
})


