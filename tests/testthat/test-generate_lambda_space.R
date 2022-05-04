test_that("all points in lambda space are unique", {
  expect_equal(nrow(unique(generate_lambda_space(8))), 2^8)
})
