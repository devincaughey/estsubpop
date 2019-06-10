#' @title Fit a dynamic ecological inference model
#'
#' @description This is the workhorse function of the \pkg{estsubpop}
#'     package. Based on the data contained in \code{design_ls} and other
#'     arguments, it writes a Stan script describing the model and then calls
#'     the \code{\link[rstan]{stan}} function from the \pkg{rstan} package to
#'     sample from the model using Hamiltonian Monte Carlo.
#'
#' @param design_ls List of survey design objects, as created by the
#'     \code{\link[survey]{svydesign}} function from the \pkg{survey} package.
#' @param formulae_ls List of formulas, in the same order as \code{design_ls},
#'     indicating the marginal or joint distrbutions to be extracted from the
#'     corresponding survey design object . Following the usage of the
#'     \pkg{survey} package, \code{~x1} will extract the marginal distribution
#'     of variable \code{x1}, and \code{~x1 + x2} will extract the joint
#'     distribution of \code{x1} and \code{x2}.
#' @param periods_to_est Vector containing the names of the time periods for
#'     which estimates are desired.
#' @param period_var Name of the variable indicating time periods (default:
#'     \code{"YEAR"})
#' @param n_sample A \eqn{Y}-by-\eqn{M} array, where \eqn{Y} is the length of
#'     \code{periods_to_est} and \eqn{M} is the maximum number of formulas in
#'     \code{formulae_ls} in any period. The value of each element of
#'     \code{n_sample} indicates the sample size of the corresponding
#'     dataset. If \code{NULL}, the default, then the sample sizes will be
#'     calculated from \code{design_ls}, using the function
#'     \code{\link{make_n_sample}}.
#' @param n_prior The "sample size" of the Dirichlet priors for the cell
#'     proportions.  Either \code{"vague"} (the default) or a positive
#'     scalar. If \code{"vague"}, the prior sample size is set to the inverse of
#'     the smallest element of \eqn{\boldsymbol{pi}_0}, the vector of prior
#'     means. Setting \code{pi_prior} to \code{"Jeffreys"} overrides the value
#'     of \code{n_prior}.
#' @param n_evolve_meanlog Mean of the normal prior for log(\eqn{n^{evol}}).
#' @param n_evolve_sdlog Standard deviation of the normal prior for
#'     log(\eqn{n^{evol}}). If \code{NULL}, then \eqn{n^{evol}} is assigned the
#'     value exp(\code{n_evolve_meanlog}) rather than modeled with a prior.
#' @param pi_prior A character, one of \code{"Jeffreys"}, \code{"vague"}, or
#'     \code{"raked"} (the default). If \code{"Jeffreys"}, the
#'     \eqn{N} first-period cell proportions are given the Jeffreys prior
#'     Dir(\eqn{1/2 * 1_N}), where \eqn{1_N} is an \eqn{N}-vector of 1s. If
#'     \code{"vague"}, they are given the prior Dir(\code{n_prior}\eqn{ * 1/N *
#'     1_N}). If \code{"raked"}, the prior means (rather than being uniform) are
#'     determined by raking the table of \eqn{N} cells to match the target
#'     proportions for the first year with auxiliary data.
#' @param gaps A one-dimensional array of length \eqn{Y} indicating the number
#'     of time periods between each year to estimate. If \code{NULL} (the
#'     default), the gaps will be calculated automatically from the auxiliary
#'     data.
#' @param sampling_model One of \code{"dirichlet"} (the default) or
#'     \code{"multinomial"}.
#' @param transition_model One of \code{"dirichlet"} (the default) or
#'     \code{"logistic-normal"} (not yet implemented).
#' @param verbosity An integer. If greater than or equal to 1, the Stan code for
#'     the model will be printed.
#' @param chains An integer indicating the number of Monte Carlo chains (passed
#'     to \code{\link[rstan]{stan}}).
#' @param iter An integer indicating the number of Monte Carlo iterations in
#'     each chain (passed to \code{\link[rstan]{stan}}).
#' @param refresh An integer indicating how frequently the progress of the
#'     sampler should be reported---i.e., show the progress every \code{refresh}
#'     iterations (passed to \code{\link[rstan]{stan}}).
#' @param thin A positive integer specifying the period for saving samples
#'     (passed to \code{\link[rstan]{stan}}).
#' @param stan_code A character string of Stan code encoding the desired
#'     model. If \code{NULL}, the code will be written automatically.
#' @param ... Further arguments passed to \code{\link[rstan]{stan}}.
#'
#' @seealso Devin Caughey and Mallory Wang. 2019. “Dynamic Ecological Inference
#'     for Time-Varying Population Distributions Based on Sparse, Irregular, and
#'     Noisy Marginal Data.” \emph{Political Analysis} 27 (3):
#'     388–396. <https://doi.org/10.1017/pan.2019.4>. 
#' 
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
                        verbosity = 1L,
                        chains = 4L,
                        iter = 1e4,
                        refresh = 100,
                        thin = 1L,
                        stan_code = NULL,
                        ...) {
    aux_info <- estsubpop::make_target_ls(design_ls,
                                          formulae_ls,
                                          periods_to_est, 
                                          period_var = period_var)
    if (is.null(n_sample)) {
        n_sample <- estsubpop::make_n_sample(aux_info)
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
    stan_data <- do.call(estsubpop::make_stan_data, data_args)
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
