test_that("gradient works", {
  n <- 4
  m <- 2
  r <- 5
  mods <- list(mod1 = 1:3,
               mod2 = c(1, 4, 5))
  # KO and module matrix
  KO_mod_mat <- matrix(0, nrow = r, ncol = m)
  # fill in KO and module matrix
  for (k in 1:m) {
    KO_mod_mat[mods[[k]], k] <- 1
  }
  x <- matrix(c(rep(1, 4), c(1, 0, 0, 1)), nrow = 4)
  y <- matrix(c(1,0,1,0,0,0,1,1,0,1,1,0,1,1,1,0,1,0,0,1), nrow = 4)
  beta <- matrix(c(1, 1, 10, -10), nrow = 2, byrow = TRUE)
  expect_equal(round(compute_gradient(0.8, 0.1, beta, x, y, KO_mod_mat), 3),
               round(c(2.688718e+01, -3.463589e+01, -2.842205e-01,
                 -1.262052e-01, 5.345756e-05, -7.811186e-03), 3))
})
