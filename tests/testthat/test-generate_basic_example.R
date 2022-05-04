test_that("x dimension correct", {
  expect_equal(dim(generate_basic_example()$x), c(4, 2))
})

test_that("y dimension correct", {
  expect_equal(dim(generate_basic_example()$y), c(4, 5))
})
