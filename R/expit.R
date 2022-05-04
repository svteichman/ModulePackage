#' Expit
#'
#' Computes the expit of a value
#'
#' @param x A real number.
#'
#' @return A number representing the expit of \code{x}. This is equal to
#' \code{1/(1+e^(-x))}.
#'
#' @examples
#' expit(1)
#' expit(1000)
#' expit(-1000)
#'
#' @export
expit <- function(x) {
  return(1/(1+exp(-x)))
}
