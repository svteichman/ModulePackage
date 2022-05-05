#' Sums Over Sample Space for Lambda
#'
#' This function computes the portion of the likelihood for a given sample \code{i} over all
#' points in the sample space for \code{lambda_i}.
#'
#' @param a A number in \code{[0, 1]} that represents the probability of observing a KO given
#' that one of the modules it is included in is present.
#' @param epsilon A number in \code{[0, 1]} that represents the probability of observing a
#' KO given that none of the modules it is included in are present.
#' @param beta A matrix with \code{p+1} rows and \code{m} columns. The first row are
#' intercepts for each module, and the element in row \code{i} and column \code{k} represents
#' the coefficient for the covariate \code{x_{i+1}} and module \code{k}.
#' @param x A design matrix, where the first column contains \code{1}'s and the next \code{p}
#' columns include the \code{p} covariates measured on the \code{n} samples.
#' @param y The outcome matrix, with \code{n} rows and \code{r} columns where the element in
#' row \code{i} and column \code{j} is \code{1} if KO \code{j} was observed in sample \code{i}
#' and \code{0} otherwise.
#' @param sample_space The sample space for the lambda vector. Each of the \code{2^m} rows
#' represents a point in the space and each of the \code{m} columns represents a module.
#' @param i A number representing the sample being considered.
#' @param KO_max_mat A matrix with \code{2^m} rows and \code{r} columns. Each row represents
#' a point in the sample space of \code{lambda} and each column represents a KO. The element
#' in the row \code{ss} and column \code{j} is \code{1} if for the point in the sample space
#' given by \code{ss}, the maximum over all \code{lambda} values with indices in the set
#' \code{D_j} is \code{1}, and \code{0} otherwise.
#' @param use_expit If \code{TRUE}, then replace \code{a} and \code{epsilon} with \code{expit(a)}
#' and \code{expit(epsilons)}. Set to \code{FALSE} by default.
#'
#' @return The likelihood product for the given sample \code{i}.
#'
#' @examples
#' dat <- generate_basic_example()
#' beta <- matrix(rnorm(4), nrow = 2)
#' sample_space <- generate_lambda_space(2)
#' KO_max_mat <- transform_lambda_space(2, 5, sample_space, dat$KO_mod_mat)
#' sum_lambda_space(0.8, 0.1, beta, dat$x, dat$y, sample_space, 1, KO_max_mat)
#'
#' @export
sum_lambda_space <- function(a, epsilon, beta, x, y, sample_space, i, KO_max_mat, use_expit = FALSE) {
  # initialize sum
  lik_sum <- 0
  # add likelihood for each point in sample space of lambda for sample i
  for (ss in 1:nrow(sample_space)) {
    lik_sum <- lik_sum +
      compute_lik_prod(a, epsilon, beta, x, y,
                       sample_space[ss, ], i, KO_max_mat[ss, ], use_expit)
  }
  return(lik_sum)
}
