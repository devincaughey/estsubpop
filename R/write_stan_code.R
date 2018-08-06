#' Write Stan code for dynamic ecological inference model
#'
#' Given information on the variable subsets in each time period,
#' \code{write_stan_code} writes Stan code for estimating the corresponding
#' model. 
#'
#' @param x Either (a) a matrix in which the element in row \eqn{t}, column
#'     \eqn{m} indicates the number of groups (\eqn{G_{tm}}) in proportion
#'     vector \eqn{m} in time period \eqn{t} (\eqn{p_{tm}}) or (b) a list of
#'     population targets from which the matrix above can be created.
#' @param vebosity Scalar; if greater than 1, the stan code will be printed
#' @param sampling_model Either "multinomial" or "dirichlet"
#' @param est_n_evolve If TRUE, then \eqn{n^{evol}} will modeled as a parameter
#'     rather than treated as data.
#' 
#' @return a character string
#' 
#' @export

write_stan_code <- function (x, est_n_evolve = FALSE, verbosity = 1L,
                             transition_model = "dirichlet",
                             sampling_model = "dirichlet") {
    stopifnot(sampling_model %in% c("multinomial", "dirichlet"))
    stopifnot(transition_model %in% c("logistic-normal", "dirichlet"))
    if (is.list(x)) LL <- estsubpop::make_LL(x) else LL <- x
    Y <- nrow(LL)
    stan_code_to_add <- character(2)
    names(stan_code_to_add) <- c("data", "model")
    for (y in 1:Y) {
        (M_y <- sum(LL[y, ] > 0))       # number of margins observed in period y
        if (M_y == 0) next              # if none, skip year
        for (m in 1:M_y) {
            (inm <- paste0("A_y", y, "m", m))
            if (identical(sampling_model, "multinomial")) {
                (cnm <- paste0("counts_y", y, "m", m))
                (dc <- paste0("  int<lower=0> ", cnm, "[", LL[y, m], "];\n",
                              "  matrix<lower=0,upper=1>[", LL[y, m], ", N] ",
                              inm, ";"))
                (mc <- paste0("  ", cnm, " ~ multinomial(", inm, " * pi[", y,
                              "]);"))
            }
            if (identical(sampling_model, "dirichlet")) {
                (cnm <- paste0("props_y", y, "m", m))
                (dc <- paste0("  simplex[", LL[y, m], "] ", cnm, ";\n",
                              "  matrix<lower=0,upper=1>[", LL[y, m], ", N] ",
                              inm, ";"))
                (mc <- paste0("  ", cnm, " ~ dirichlet(n_sample[", y, ", ", m,
                              "] * ", inm, " * pi[", y, "]);"))
            }
            stan_code_to_add["data"] <-
                paste(stan_code_to_add["data"], dc, sep = "\n")
            stan_code_to_add["model"] <-
                paste(stan_code_to_add["model"], mc, sep = "\n")
        }
    }
    if (isTRUE(est_n_evolve)) {
        stopifnot(identical(transition_model, "dirichlet"))
        n_evolve_data_code <- "
  real n_evolve_meanlog;
  real<lower=0> n_evolve_sdlog;"
        n_evolve_param_code <- "
  real<lower=0> n_evolve;"
        n_evolve_model_code <- "
  n_evolve ~ lognormal(n_evolve_meanlog, n_evolve_sdlog);"
    } else {
        if (identical(transition_model, "dirichlet")) {
            n_evolve_data_code <- "\n  real<lower=0> n_evolve;"
            n_evolve_param_code <- ""
            n_evolve_model_code <- ""
        }
        if (identical(transition_model, "logistic-normal")) {
            
        }
    }
    stan_code <- paste0(
        "\ndata {
  int<lower=1> N; // number of cells
  int<lower=1> Y; // number of time periods
  int<lower=1> M; // maximum number of margins in any period
  matrix<lower=0>[Y, M] n_sample;
  real<lower=0> n_prior;", n_evolve_data_code,
  "
  real Ygaps[Y]; // no. of periods skipped between estimated periods
  simplex[N] pi0;", stan_code_to_add["data"], "
}
parameters {
  simplex[N] pi[Y]; // period-specific cell probs", n_evolve_param_code, "
}
model {", n_evolve_model_code, "
  pi[1] ~ dirichlet(pi0 * n_prior);
  for (y in 2:Y) {
    pi[y] ~ dirichlet(pi[y - 1] * n_evolve / Ygaps[y]);
  }", stan_code_to_add["model"], "
}\n")
    if (verbosity >= 1) cat(stan_code)
    invisible(stan_code)
}
