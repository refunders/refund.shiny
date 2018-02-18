library(refund)

context("fpca plots")


test_that("no errors arise for registr::bfpca",{
  skip_on_travis()
  library(registr)
  fpca_data = simulate_functional_data(I = 50, D = 20, seed = 343)
  fpca_registr = bfpca(fpca_data$Y, print.iter  = FALSE, npc = 3)
  expect_error(plot_shiny(fpca_registr), NA)

})

test_that("no errors arise for refund::fpca.sc",{
  data(cd4)
  fpca_sc = fpca.sc(cd4)
  expect_error(plot_shiny(fpca_sc), NA)
})

test_that("no errors arise for refund::fpca.face or refund::fpca.ssvd",{

  set.seed(2678695)
  n = 101
  m = 101
  s1 = 20
  s2 = 10
  s = 4
  t = seq(-1, 1, l=m)
  v1 = t + sin(pi*t)
  v2 = cos(3*pi*t)
  V = cbind(v1/sqrt(sum(v1^2)), v2/sqrt(sum(v2^2)))
  U = matrix(rnorm(n*2), n, 2)
  D = diag(c(s1^2, s2^2))
  eps = matrix(rnorm(m*n, sd=s), n, m)
  Y = U%*%D%*%t(V) + eps

  fpca_face = fpca.face(Y)
  expect_error(plot_shiny(fpca_face), NA)

  fpca_ssvd = fpca.ssvd(Y, verbose=FALSE)
  expect_error(plot_shiny(fpca_ssvd), NA)
})


