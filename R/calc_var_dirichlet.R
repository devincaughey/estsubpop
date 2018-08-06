#' Calculate the sampling variance of an element of a Dirichlet-distributed
#' simplex
#'
#' Given a Dirichlet distribution whose parameter vector \eqn{\alpha} sums to
#' \code{n_total}, \code{calc_var_dirichlet} calculates the sampling variance
#' of an element \eqn{i} for which \eqn{\alpha_i = } \code{prop * n_total},
#' using the formula \code{(prop * (1 - prop)) / (n_total + 1)}.
#'
#' @param prop stipulated proportion
#' @param n_total the ``sample size'' of the Dirichlet distribution, i.e.,
#' \eqn{\sum \alpha_i}.
#' 
#' @return The sampling variance of the proportion
#' 
#' @examples
#' ## standard error of a proportion of 0.5 given a sample size of 2,499
#' sqrt(calc_var_dirichlet(0.5, 2499))
#'
#' @export

calc_var_dirichlet <- function (prop, n_total) {
  (prop * (1 - prop)) / (n_total + 1)
}
