#' Write `cmdstanr`-style Stan code for dynamic ecological inference model
#'
#' Given information on the variable subsets in each time period,
#' \code{write_stan_code} writes Stan code for estimating the corresponding
#' model with `cmdstanr`.
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
                             sampling_model = "multinomial") {
    stopifnot(sampling_model %in% c("multinomial", "dirichlet"))
    stopifnot(transition_model %in% c("logistic-normal", "dirichlet"))
    if (is.list(x)) 
        LL <- estsubpop::make_LL(x)
    else LL <- x
    Y <- nrow(LL)
    stan_code_to_add <- character(3)
    names(stan_code_to_add) <- c("data", "transformed data", "model")
    for (y in 1:Y) {
        (M_y <- sum(LL[y, ] > 0))
        if (M_y == 0) 
            next
        for (m in 1:M_y) {
            (nAym <- paste0("nA_", y, "m", m))
            (Aym <- paste0("A_y", y, "m", m))
            gym <- paste0("G_y", y, "m", m)
            gc <- paste0("  int<lower=1> gym;")
            if (identical(sampling_model, "multinomial")) {
                (cym <- paste0("counts_y", y, "m", m))
                (dc <- paste0(
                     gc,
                     "\n  array[", gym, "] int<lower=0> ", cym,
                     ";\n", "  matrix<lower=0,upper=1>[",
                     gym, ", N] ", Aym, ";"))
                (mc <- paste0("  ", cym, " ~ multinomial(", Aym, 
                              " * pi[", y, "]);"))
            }
            if (identical(sampling_model, "dirichlet")) {
                (cym <- paste0("props_y", y, "m", m))
                (dc <- paste0(
                     gc,
                     "\n  simplex[", gym, "] ", cym, 
                     ";\n", "  matrix<lower=0,upper=1>[", gym,
                     ", N] ", Aym, ";"))
                (mc <- paste0(
                     "  profile(\"likelihood_", y, "_", m, "\") {",
                     "\n    target += dirichlet_lpdf(", cym, " | ",
                     nAym, " * pi[", y, "]);", "\n  }"
                 ))
            }
            tdc <- paste0(
                "    matrix[", gym, ", N] ", nAym,
                " = n_sample[", y, ", ", m, "] * ", Aym, ";"
            )
            stan_code_to_add["data"] <-
                paste(stan_code_to_add["data"], dc, sep = "\n")
            stan_code_to_add["transformed data"] <-
                paste(stan_code_to_add["transformed data"], tdc, sep = "\n")
            stan_code_to_add["model"] <-
                paste(stan_code_to_add["model"], mc, sep = "\n")
            
        }
    }
    if (isTRUE(est_n_evolve)) {
        stopifnot(identical(transition_model, "dirichlet"))
        n_evolve_data_code <-
            "\n  real n_evolve_meanlog;\n  real<lower=0> n_evolve_sdlog;"
        n_evolve_param_code <-
            "\n  real<lower=0> n_evolve;"
        n_evolve_model_code <-
            "\n  n_evolve ~ lognormal(n_evolve_meanlog, n_evolve_sdlog);"
    }
    else {
        if (identical(transition_model, "dirichlet")) {
            n_evolve_data_code <- "\n  real<lower=0> n_evolve;"
            n_evolve_param_code <- ""
            n_evolve_model_code <- ""
        }
        if (identical(transition_model, "logistic-normal")) {
        }
    }
    stan_code <- paste0(
        "\ndata {",
        "\n  int<lower=1> N; // number of cells",
        "\n  int<lower=1> Y; // number of time periods",
        "\n  int<lower=1> M; // maximum number of margins in any period",
        "\n  matrix<lower=0>[Y, M] n_sample;",
        "\n  real<lower=0> n_prior;", 
        n_evolve_data_code,
        "\n  array[Y] real Ygaps; // no. of periods skipped between estimated periods",
        "\n  simplex[N] pi0;", 
        stan_code_to_add["data"],
        "\n}",
        "\ntransformed data {",
        stan_code_to_add["transformed data"],
        "\n}",
        "\nparameters {",
        "\n  array[Y] simplex[N] pi; // period-specific cell probs", 
        n_evolve_param_code,
        "\n}",
        "\nmodel {", n_evolve_model_code,
        "\n  pi[1] ~ dirichlet(pi0 * n_prior);",
        "\n  for (y in 2:Y) {",
        "\n    pi[y] ~ dirichlet(pi[y - 1] * n_evolve / Ygaps[y]);",
        "\n  }", 
        stan_code_to_add["model"],
        "\n}",
        "\n")
    if (verbosity >= 1) 
        cat(stan_code)
    invisible(stan_code)
}
