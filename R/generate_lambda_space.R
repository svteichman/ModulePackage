#' Generate lambda space
#'
#' This function generates the sample space for the vector lambda.
#'
#' @param m The number of modules in the dataset.
#'
#' @return A matrix with \code{2^m} rows and \code{m} columns. Each row represents one
#' point in the sample space for \code{lambda}. Each column contains a \code{1} for
#' modules that are present and a \code{0} for modules that are absent for that point in
#' the sample space.
#'
#' @examples
#' generate_lambda_space(4)
#'
#' @export
generate_lambda_space <- function(m) {
  sample_space <- matrix(nrow = 2^m, ncol = m)
  for (k in 1:m) {
    sample_space[, k] <- rep(c(rep(0, 2^(m-k)), rep(1, 2^(m-k))), 2^(k - 1))
  }
  return(sample_space)
}
