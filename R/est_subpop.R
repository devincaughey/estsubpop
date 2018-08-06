#' @export

est_subpop <- function (design_ls,
                        formulae_ls,
                        periods_to_est,
                        period_var = "YEAR",
                        n_sample = NULL,
                        n_prior = NULL,
                        n_evolve_meanlog = 10,
                        n_evolve_sdlog = NULL,
                        pi_prior = "raked",
                        gaps = NULL,
                        sampling_model = "dirichlet",
                        transition_model = "dirichlet",
                        verbosity = 1,
                        chains = 4,
                        iter = 1e4,
                        refresh = 100,
                        thin = 10,
                        stan_code = NULL,
                        ...) {
    aux_info <- make_target_ls(design_ls, formulae_ls, periods_to_est,
                               period_var = period_var)
    if (is.null(n_sample)) {
        n_sample <- make_n_sample(aux_info)
    }
    data_args <- list(
        n_evolve_meanlog = n_evolve_meanlog,
        n_evolve_sdlog = n_evolve_sdlog,
        n_sample = n_sample,
        n_prior = n_prior,
        pi_prior = pi_prior,
        gaps = gaps,
        target_ls = aux_info,
        sampling_model = sampling_model)
    stan_data <- do.call(make_stan_data, data_args)
    if (is.null(stan_code)) {
        stan_code <- write_stan_code(
            aux_info,
            sampling_model = sampling_model,
            verbosity = verbosity,
            est_n_evolve = !is.null(n_evolve_sdlog))
    }
    stan_out <- stan(data = stan_data,
                     model_code = stan_code,
                     chains = chains,
                     iter = iter,
                     refresh = refresh,
                     thin = thin,
                     ...)
    return(list(stan_out = stan_out,
                stan_data = stan_data,
                aux_info = aux_info,
                periods = periods_to_est))
}
