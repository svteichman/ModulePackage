test_that("lambda space is transformed appropriately", {
  sample_space <- generate_lambda_space(2)
  KO_mod_mat <- matrix(c(1, 1, 1, 0, 1, 0, 0, 1, 0, 1), nrow = 5, byrow = TRUE)
  res <- transform_lambda_space(2, 5, sample_space, KO_mod_mat)
  expect_equal(c(res[1, 1], res[2, 2], res[3, 3], res[4, 4]), c(0, 0, 1, 1))
})
