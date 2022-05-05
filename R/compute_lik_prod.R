#' Compute Piece of Likelihood
#'
#' This function computes the portion of the likelihood for a given sample \code{i} and for
#' a given point in the sample space for \code{lambda_i}.
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
#' @param lambda A vector of \code{lambda} values representing one point in the sample space
#' in which element \code{k} is \code{1} if module \code{k} is present in sample \code{i} and
#' \code{0} otherwise.
#' @param i A number representing the sample being considered.
#' @param KO_max A vector of length \code{r} based on the value lambda in which a \code{1}
#' for element \code{j} means that the maximum over all lambda values with indices in the
#' set \code{D_j} is \code{1}, and a \code{0} means that the maximum is \code{0}.
#' @param use_expit If \code{TRUE}, then replace \code{a} and \code{epsilon} with \code{expit(a)}
#' and \code{expit(epsilons)}. Set to \code{FALSE} by default.
#'
#' @return The likelihood product for the given sample \code{i} and lambda vector.
#'
#' @examples
#' beta <- matrix(rnorm(4), nrow = 2)
#' x <- matrix(c(rep(1, 5), rbinom(5, 1, 0.5)), nrow = 5)
#' y <- matrix(c(rbinom(5*5, 1, 0.5)), nrow = 5)
#' lambda <- c(1, 0)
#' KO_max <- c(1, 1, 1, 0, 0)
#' compute_lik_prod(0.8, 0.1, beta, x, y, lambda, 2, KO_max)
#'
#' @export
compute_lik_prod <- function(a, epsilon, beta, x, y, lambda, i, KO_max, use_expit = FALSE) {
  # number of modules
  m <- length(lambda)
  # number of KOs
  r <- length(KO_max)
  # get vector of X[i, ] %*% beta values
  x_beta_vec <- x[i, ] %*% beta
  # initialize product
  prod <- 1
  # product over term in the likelihood that includes a and epsilon
  if (use_expit) {
    a <- expit(a)
    epsilon <- expit(epsilon)
  }
  for (j in 1:r) {
    val <- (a^y[i, j]*(1-a)^(1-y[i, j]))^(KO_max[j])*
      (epsilon^y[i, j]*(1-epsilon)^(1-y[i, j]))^(1-KO_max[j])
    prod <- prod*val
  }
  # product over term in the likelihood that includes x and beta
  for (k in 1:m) {
    val <- expit(x_beta_vec[k])^lambda[k]*
      (1-expit(x_beta_vec[k]))^(1-lambda[k])
    prod <- prod*val
  }
  # return portion of the likelihood for that sample i and lambda value
  return(prod)
}
