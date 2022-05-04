#' Generate Basic Example
#'
#' Generates data to test that functions are working as expected
#'
#' @return A list including \code{x}, \code{y}, and \code{KO_mod_mat}.
#'
#' @examples
#' generate_basic_example()
#'
#' @export
generate_basic_example <- function() {
  # 4 samples
  n <- 4
  # 2 modules
  m <- 2
  # 5 KOs
  r <- 5
  # mod 1 contains KOs 1, 2, 3, mod 2 contains KOs 1, 4, 5
  mods <- list(mod1 = 1:3,
               mod2 = c(1, 4, 5))
  # KO and module matrix
  KO_mod_mat <- matrix(0, nrow = r, ncol = m)
  # fill in KO and module matrix
  for (k in 1:m) {
    KO_mod_mat[mods[[k]], k] <- 1
  }
  # design matrix with intercept column and one covariate
  x <- matrix(c(rep(1, n), c(1, 0, 0, 1)), ncol = 2)
  # outcome matrix
  y <- matrix(c(1,0,1,0,0,0,1,1,0,1,1,0,1,1,1,0,1,0,0,1), nrow = 4)
  # wrap up data and module definitions in a list
  res <- list(x = x,
              y = y,
              KO_mod_mat = KO_mod_mat)
  return(res)
}
