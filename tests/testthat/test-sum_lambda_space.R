test_that("correct sum", {
  dat <- generate_basic_example()
  beta <- matrix(1:4, nrow = 2)
  sample_space <- generate_lambda_space(2)
  KO_max_mat <- transform_lambda_space(2, 5, sample_space, dat$KO_mod_mat)
  res <- sum_lambda_space(0.8, 0.1, beta, dat$x, dat$y, sample_space, 2, KO_max_mat)
  y <- dat$y
  x <- dat$x
  # compute product by hand
  sum_res <- 0.1^y[2, 1] * 0.9^(1 - y[2, 1]) * 0.1^y[2, 2] * 0.9^(1 - y[2, 2]) *
    0.1^y[2, 3] * 0.9^(1 - y[2, 3]) * 0.1^y[2, 4] * 0.9^(1 - y[2, 4]) *
    0.1^y[2, 5] * 0.9^(1 - y[2, 5]) *
    (1 - expit(x[2, ] %*% beta[, 1])) * (1 - expit(x[2, ] %*% beta[, 2])) +
    0.8^y[2, 1] * 0.2^(1 - y[2, 1]) * 0.1^y[2, 2] * 0.9^(1 - y[2, 2]) *
    0.1^y[2, 3] * 0.9^(1 - y[2, 3]) * 0.8^y[2, 4] * 0.2^(1 - y[2, 4]) *
    0.8^y[2, 5] * 0.2^(1 - y[2, 5]) *
    (1 - expit(x[2, ] %*% beta[, 1])) * (expit(x[2, ] %*% beta[, 2])) +
    0.8^y[2, 1] * 0.2^(1 - y[2, 1]) * 0.8^y[2, 2] * 0.2^(1 - y[2, 2]) *
    0.8^y[2, 3] * 0.2^(1 - y[2, 3]) * 0.1^y[2, 4] * 0.9^(1 - y[2, 4]) *
    0.1^y[2, 5] * 0.9^(1 - y[2, 5]) *
    (expit(x[2, ] %*% beta[, 1])) * (1 - expit(x[2, ] %*% beta[, 2])) +
    0.8^y[2, 1] * 0.2^(1 - y[2, 1]) * 0.8^y[2, 2] * 0.2^(1 - y[2, 2]) *
    0.8^y[2, 3] * 0.2^(1 - y[2, 3]) * 0.8^y[2, 4] * 0.2^(1 - y[2, 4]) *
    0.8^y[2, 5] * 0.2^(1 - y[2, 5]) *
    (expit(x[2, ] %*% beta[, 1])) * (expit(x[2, ] %*% beta[, 2]))
  expect_equal(res, as.numeric(sum_res))
})
