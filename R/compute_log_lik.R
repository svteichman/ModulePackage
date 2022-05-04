#' Computes Negative Log Likelihood
#'
#' This function computes the negative log likelihood for the parameters given the data.
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
#' @param KO_mod_mat A matrix with \code{r} rows and \code{m} columns that encodes module
#' definitions, where a \code{1} for row \code{j} and column \code{k} means that KO \code{j}
#' is in the definition for module \code{k}.
#'
#' @return The negative log likelihood of the parameters given the data.
#'
#' @examples
#' dat <- generate_basic_example()
#' beta <- matrix(rnorm(4), nrow = 2)
#' compute_log_lik(0.8, 0.1, beta, dat$x, dat$y, dat$KO_mod_mat)
#'
#' @export
compute_log_lik <- function(a, epsilon, beta, x, y, KO_mod_mat) {
  # number of modules
  m <- ncol(KO_mod_mat)
  # number of KOs
  r <- nrow(KO_mod_mat)
  # create sample space for lambda values
  sample_space <- generate_lambda_space(m)
  # compute max lambda for each KO for each point in sample space
  KO_max_mat <- transform_lambda_space(m, r, sample_space, KO_mod_mat)
  # initialize log likelihood
  log_lik <- 0
  # sample size
  n <- nrow(x)
  # take sum of log likelihood for each sample
  for (i in 1:n) {
    log_lik <- log_lik +
      log(sum_lambda_space(a, epsilon, beta, x, y,
                                    sample_space, i, KO_max_mat))
  }
  return(-log_lik)
}
