library(registr)
library(refund)

context("registration plots")


test_that("no errors arise for binary registration plots",{
  registration_data = simulate_unregistered_curves(I = 20, D = 50, seed = 2018)
  binary_registration = register_fpca(Y = registration_data, family = "binomial",
                                      Kt = 6, Kh = 3, npc  = 1)
  expect_error(plot_shiny(binary_registration), NA)

  #plot_shiny(binary_registration, thin_data = TRUE)

})

test_that("no errors arise for gaussian registration plots",{
  registration_data = simulate_unregistered_curves(I = 20, D = 50, seed = 2018)
  registration_data$value = registration_data$latent_mean
  gaussian_registration = register_fpca(Y = registration_gauss, family = "gaussian")

  expect_error(plot_shiny(gaussian_registration), NA)

})

test_that("registration works with DTI data",{


  #plot_shiny(binary_registration, thin_data = TRUE)

})


test_that("no errors arise for non 0-1 data",{

})
