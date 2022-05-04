#' Computes Gradient
#'
#' The function computes the gradient of the negative log likelihood based on the input
#' data and parameter values.
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
#' @return The gradient of negative log likelihood for a certain set of parameters and data.
#'
#' @importFrom stats rbinom
#'
#' @examples
#' dat <- generate_basic_example()
#' beta <- matrix(rnorm(4), nrow = 2)
#' compute_gradient(0.8, 0.1, beta, dat$x, dat$y, dat$KO_mod_mat)
#'
#' @export
compute_gradient <- function(a, epsilon, beta, x, y, KO_mod_mat) {
  # number of modules
  m <- ncol(KO_mod_mat)
  # number of KOs
  r <- nrow(KO_mod_mat)
  # sample size
  n <- nrow(x)
  # number of covariates
  p <- ncol(x) - 1
  # initialize gradient vector to return
  grad <- rep(0, 2 + (p+1)*m)
  # create sample space for lambda values
  sample_space <- generate_lambda_space(m)
  # compute max l_{.k} for each KO for each point in sample space
  KO_max_mat <- transform_lambda_space(m, r, sample_space, KO_mod_mat)

  # gradient with respect to each parameter is sum over gradient for that sample
  for (i in 1:nrow(x)) {
    # get x[i, ] %*% beta vector
    x_beta_vec <- x[i, ] %*% beta
    denom <- 0
    num <- rep(0, length(grad))
    # loop over points in sample space for lambda
    for (ss in 1:nrow(sample_space)) {
      # get likelihood for sample i and lambda
      num_term <- compute_lik_prod(a, epsilon, beta, x, y,
                                 sample_space[ss, ], i, KO_max_mat[ss, ])
      # add num_term to denominator
      denom <- denom + num_term
      # get derivatives for a and epsilon
      a_sum <- 0
      epsilon_sum <- 0
      for (j in 1:r) {
        a_val <- KO_max_mat[ss, j]*(y[i, j]*(1-a)^(1-y[i, j])*a^(y[i, j] - 1) -
                                     (1-y[i, j])*(1-a)^(-y[i, j])*a^y[i, j])*
          ((1-a)^(1-y[i, j])*a^y[i, j])^(-1)
        a_sum <- a_sum + a_val
        epsilon_val <- (1-KO_max_mat[ss, j])*(y[i, j]*(1-epsilon)^(1-y[i, j])*
                                               epsilon^(y[i, j] - 1) -
                                               (1-y[i, j])*(1-epsilon)^(-y[i, j])*
                                               epsilon^y[i, j])*
          ((1-epsilon)^(1-y[i, j])*epsilon^y[i, j])^(-1)
        epsilon_sum <- epsilon_sum + epsilon_val
      }
      num[1] <- num[1] + num_term*a_sum
      # get derivative for e
      num[2] <- num[2] + num_term*epsilon_sum
      # get derivative for beta values
      for (k in 1:m) {
        # the additional term in the derivative for the intercept parameters
        b0_val <- sample_space[ss, k]*exp(-x_beta_vec[k])*expit(x_beta_vec[k]) -
          (1 - sample_space[ss, k])*exp(-x_beta_vec[k])*expit(x_beta_vec[k])^2*
          (1 - expit(x_beta_vec[k]))^(-1)
        num[2 + k] <- num[2 + k] + num_term*b0_val
        # the additional term in the derivative for the covariate parameters
        for (cov in 1:p) {
          b_val <- sample_space[ss, k]*x[i, cov + 1]*exp(-x_beta_vec[k])*
            expit(x_beta_vec[k]) -
            (1 - sample_space[ss, k])*x[i, cov + 1]*exp(-x_beta_vec[k])*
            expit(x_beta_vec[k])^2*(1 - expit(x_beta_vec[k]))^(-1)
          num[2 + m*cov + k] <- num[2 + m*cov + k] + num_term*b_val
        }
      }
    }
    grad <- grad + num/denom
  }
  return(-grad)
}
