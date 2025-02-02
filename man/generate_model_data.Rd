% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/generate_model_data.R
\name{generate_model_data}
\alias{generate_model_data}
\title{Generate Data from Model}
\usage{
generate_model_data(
  seed = NULL,
  n,
  m,
  r,
  KO_mod_mat = NULL,
  p,
  a,
  epsilon,
  beta,
  use_expit = FALSE,
  cont_x = FALSE
)
}
\arguments{
\item{seed}{An optional seed to set.}

\item{n}{The sample size.}

\item{m}{The number of modules.}

\item{r}{The number of KOs.}

\item{KO_mod_mat}{A matrix describing module definitions. If this argument is left as NULL
then this will be randomly generated based on the data.}

\item{p}{The number of covariates.}

\item{a}{A number in \code{[0, 1]} that represents the probability of observing a KO given
that one of the modules it is included in is present.}

\item{epsilon}{A number in \code{[0, 1]} that represents the probability of observing a
KO given that none of the modules it is included in are present.}

\item{beta}{A matrix with \code{p+1} rows and \code{m} columns. The first row are
intercepts for each module, and the element in row \code{i} and column \code{k} represents
the coefficient for the covariate \code{x_{i+1}} and module \code{k}.}

\item{use_expit}{If \code{TRUE}, then replace \code{a} and \code{epsilon} with \code{expit(a)}
and \code{expit(epsilons)}. Set to \code{FALSE} by default.}

\item{cont_x}{If \code{TRUE}, then generate continuous variables in
design matrix \code{x} instead of binary. Set to \code{FALSE} by default.}
}
\value{
A list including \code{x}, \code{y}, and \code{KO_mod_mat}.
}
\description{
Generates data from generative model.
}
\examples{
beta <- matrix(c(rep(1, 2), 1, 5), nrow = 2)
generate_model_data(n = 4, m = 2, r = 5, p = 1, a = 0.8, epsilon = 0.1, beta = beta)

}
