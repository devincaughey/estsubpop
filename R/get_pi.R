#' @export

get_pi <- function (est_obj) {
  pi <- rstan::extract(est_obj$stan_out, pars = "pi", permuted = FALSE)
  estsubpop::melt_pi(pi_array = pi,
                     yr_names = est_obj$periods,
                     xtab = estsubpop::make_XX(est_obj$aux_info))
}
