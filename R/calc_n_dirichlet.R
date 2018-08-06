#' @export

calc_n_dirichlet <- function (prop, prop_variance) {
  (prop * (1 - prop) / prop_variance) - 1
}
