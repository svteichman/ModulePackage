#' Transform lambda space
#'
#' This function takes the sample space for the vector lambda and transforms it into the
#' sample space for the maximum lambda values over all indices in the set \code{D_j} for
#' each KO \code{j}
#'
#' @param m The number of modules in the dataset.
#' @param r The number of KOs in the dataset.
#' @param sample_space The sample space for the lambda vector.
#' @param KO_mod_mat A matrix with \code{r} rows and \code{m} columns that encodes module
#' definitions, where a \code{1} for row \code{j} and column \code{k} means that KO \code{j}
#' is in the definition for module \code{k}.
#'
#' @return A matrix with \code{2^m} rows and \code{r} columns. Each row represents one
#' point in the sample space for \code{lambda}. Each column contains a \code{1} for
#' KOs for which one of the modules they are included in is present and a \code{0} for KOs
#' for which none of the modules they are included in are present.
#'
#' @examples
#' sample_space <- generate_lambda_space(2)
#' KO_mod_mat <- matrix(c(1, 1, 1, 0, 1, 0, 0, 1, 0, 1), nrow = 5, byrow = TRUE)
#' transform_lambda_space(2, 5, sample_space, KO_mod_mat)
#'
#' @export
transform_lambda_space <- function(m, r, sample_space, KO_mod_mat) {
  KO_max_mat <- matrix(nrow = 2^m, ncol = r)
  # loop over points in sample space
  for (ss in 1:nrow(sample_space)) {
    # loop over KOs
    for (j in 1:r) {
      KO_max_mat[ss, j] <- max(sample_space[ss, KO_mod_mat[j, ] == 1])
    }
  }
  return(KO_max_mat)
}
