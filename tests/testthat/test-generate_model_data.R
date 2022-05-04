test_that("x, y, and KO_mod_mat are all matrices", {
  beta <- matrix(c(rep(1, 2), 1, 5), nrow = 2)
  res <- generate_model_data(n = 4, m = 2, r = 5, p = 1, a = 0.8, epsilon = 0.1, beta = beta)
  expect_true(is.matrix(res$x) & is.matrix(res$y) & is.matrix(res$KO_mod_mat))
})

test_that("x has correct dimensions", {
  beta <- matrix(c(rep(1, 2), 1, 5), nrow = 2)
  res <- generate_model_data(n = 4, m = 2, r = 5, p = 1, a = 0.8, epsilon = 0.1, beta = beta)
  expect_equal(dim(res$x), c(4, 2))
})

test_that("y has correct dimensions", {
  beta <- matrix(c(rep(1, 2), 1, 5), nrow = 2)
  res <- generate_model_data(n = 4, m = 2, r = 5, p = 1, a = 0.8, epsilon = 0.1, beta = beta)
  expect_equal(dim(res$y), c(4, 5))
})

test_that("KO_mod_mat has correct dimensions", {
  beta <- matrix(c(rep(1, 2), 1, 5), nrow = 2)
  res <- generate_model_data(n = 4, m = 2, r = 5, p = 1, a = 0.8, epsilon = 0.1, beta = beta)
  expect_equal(dim(res$KO_mod_mat), c(5, 2))
})

