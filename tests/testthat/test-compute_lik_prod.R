test_that("correct product", {
  beta <- matrix(1:4, nrow = 2)
  x <- matrix(c(rep(1, 5), rbinom(5, 1, 0.5)), nrow = 5)
  y <- matrix(c(rbinom(5*5, 1, 0.5)), nrow = 5)
  lambda <- c(1, 0)
  KO_max <- c(1, 1, 1, 0, 0)
  # compute product by hand
  prod <- 0.8^y[2, 1] * 0.2^(1 - y[2, 1]) * 0.8^y[2, 2] * 0.2^(1 - y[2, 2]) *
    0.8^y[2, 3] * 0.2^(1 - y[2, 3]) * 0.1^y[2, 4] * 0.9^(1 - y[2, 4]) *
    0.1^y[2, 5] * 0.9^(1 - y[2, 5]) *
    expit(x[2, ] %*% beta[, 1]) * (1 - expit(x[2, ] %*% beta[, 2]))
  expect_equal(compute_lik_prod(0.8, 0.1, beta, x, y, lambda, 2, KO_max), as.numeric(prod))
})
