#' Generate Data from Model
#'
#' Generates data from generative model.
#'
#' @param seed An optional seed to set.
#' @param n The sample size.
#' @param m The number of modules.
#' @param r The number of KOs.
#' @param KO_mod_mat A matrix describing module definitions. If this argument is left as NULL
#' then this will be randomly generated based on the data.
#' @param p The number of covariates.
#' @param a A number in \code{[0, 1]} that represents the probability of observing a KO given
#'  that one of the modules it is included in is present.
#' @param epsilon A number in \code{[0, 1]} that represents the probability of observing a
#'  KO given that none of the modules it is included in are present.
#' @param beta A matrix with \code{p+1} rows and \code{m} columns. The first row are
#'  intercepts for each module, and the element in row \code{i} and column \code{k} represents
#'  the coefficient for the covariate \code{x_{i+1}} and module \code{k}.
#'
#' @return A list including \code{x}, \code{y}, and \code{KO_mod_mat}.
#'
#' @examples
#' beta <- matrix(c(rep(1, 2), 1, 5), nrow = 2)
#' generate_model_data(n = 4, m = 2, r = 5, p = 1, a = 0.8, epsilon = 0.1, beta = beta)
#'
#' @export
generate_model_data <- function(seed = NULL, n, m, r, KO_mod_mat = NULL, p, a, epsilon, beta) {
  # if seed if given, set it
  if (!is.null(seed)) {
    set.seed(seed)
  }
  # if module definitions aren't given in KO_mod_mat, generate them randomly
  if (is.null(KO_mod_mat)) {
    mods <- list()
    for (k in 1:m) {
      mods[[k]] <- k
    }
    names(mods) <- paste0("mod", rep(1:m))
    for (j in 1:r) {
      ind <- sample(1:m, 1, replace = FALSE)
      mods[[ind]] <- c(mods[[ind]], j)
    }
    # KO and module matrix
    KO_mod_mat <- matrix(0, nrow = r, ncol = m)
    # fill in KO and module matrix
    for (k in 1:m) {
      KO_mod_mat[mods[[k]], k] <- 1
    }
  }

  # generate binary x data from a binomial(1, 0.5) distribution
  x <- matrix(nrow = n, ncol = (p + 1))
  x[, 1] <- 1
  for (i in 1:p) {
    x[, 1 + p] <- rbinom(n, 1, 0.5)
  }

  # generate outcome data based on the generative model
  # first, generate lambda values based on covariate and beta values
  lambda <- matrix(nrow = n, ncol = m)
  for (i in 1:n) {
    for (k in 1:m) {
      prob <- expit(x[i, ] %*% beta[, k])
      lambda[i, k] <- rbinom(1, 1, prob)
    }
  }
  # next, generate y values based on two case bernoulli
  y <- matrix(nrow = n, ncol = r)
  for (i in 1:n) {
    for (j in 1:r) {
      max_val <- max(lambda[i, KO_mod_mat[j, ] == 1])
      y[i, j] <- rbinom(1, 1, a*max_val + epsilon*(1 - max_val))
    }
  }

  # wrap up data and module definitions in a list
  res <- list(x = x,
              y = y,
              KO_mod_mat = KO_mod_mat)
  return(res)
}
