#' @export

make_stan_data <- function (target_ls, n_sample = NULL, n_prior = "vague",
                            n_evolve_meanlog = 10, n_evolve_sdlog = NULL,
                            pi_prior = "raked", gaps = NULL,
                            sampling_model = "dirichlet") {
  stopifnot(sampling_model %in% c("multinomial", "dirichlet"))
  stopifnot(n_evolve_meanlog < log(.Machine$double.xmax))
  if (is.null(n_sample)) {
    stop("\nEstimated n_sample not yet implemented. Please specify a value.\n")
  }
  if (is.null(gaps)) {
    gaps <- estsubpop::make_gaps(names(target_ls))
  }
  XX <- estsubpop::make_XX(target_ls)   # matrix of all cells (full cross-tab)
  LL <- estsubpop::make_LL(target_ls)   # info on margins and groups per margin
  N <- nrow(XX)                         # number of cells
  Y <- nrow(LL)                         # number of periods to estimate
  M <- ncol(LL)                         # maximum number of margins in year
  var_name_ls <- estsubpop::get_var_names(target_ls, unlist = FALSE)
  if (identical(pi_prior, "Jeffreys")) {
    pi_prior <- rep(1 / N, N)
    cat("\nOverwriting value of 'n_prior' with Jeffreys prior",
        "(alpha_c = 1/2 for all c).\n")
    n_prior <- N / 2
  }
  if (identical(pi_prior, "uniform")) {
    pi_prior <- rep(1 / N, N)
  }
  if (identical(pi_prior, "raked")) {
    init_ds <- estsubpop::make_design(data = XX, weights = ~1)
    data1 <- target_ls[[which(!sapply(target_ls, is.null))[1]]]
    forms1 <- plyr::llply(data1, function (x) {
      estsubpop::make_formula(xvars = setdiff(names(x), "Freq"))
    })
    rake1 <- survey::rake(design = init_ds, sample.margins = forms1,
                          population.margins = data1)
    pi_prior <- 1 / rake1$prob
    pi_prior <- pi_prior / sum(pi_prior)
  }
  if (identical(n_prior, "vague")) {
    n_prior <- 1 / min(pi_prior)
  }
  if (!is.matrix(n_sample)) {
    n_sample <- matrix(n_sample, nrow = nrow(LL), ncol = ncol(LL))
  }
  margin_data <- list()
  for (y in 1:Y) {
    (M_y <- sum(LL[y, ] > 0))           # number of margins observed in period y
    if (M_y == 0) next                  # if none, skip year
    for (m in 1:M_y) {
      (A_name <- paste0("A_y", y, "m", m))
      if (identical(sampling_model, "multinomial")) {
        (cnm <- paste0("counts_y", y, "m", m))
      }
      if (identical(sampling_model, "dirichlet")) {
        (cnm <- paste0("props_y", y, "m", m))
      }
      margin_grp <- interaction(as.list(XX[var_name_ls[[y]][[m]]]), drop = TRUE)
      MM <- model.matrix(~. - 1, data = as.data.frame(margin_grp))
      colnames(MM) <- levels(margin_grp)
      margin_data[[A_name]] <- t(MM)
      if (identical(sampling_model, "multinomial")) {
        margin_data[[cnm]] <- round(n_sample[y, m] * target_ls[[y]][[m]]$Freq /
                                    sum(target_ls[[y]][[m]]$Freq))
      }
      if (identical(sampling_model, "dirichlet")) {
        margin_data[[cnm]] <-
          target_ls[[y]][[m]]$Freq / sum(target_ls[[y]][[m]]$Freq)
      }
    }
  }
  if (is.null(n_evolve_sdlog)) {
    return(c(margin_data, list(Y = Y, N = N, M = M,
                             n_sample = n_sample, n_prior = n_prior,
                             n_evolve = exp(n_evolve_meanlog),
                             pi0 = pi_prior, Ygaps = as.array(gaps))))
  } else {
    return(c(margin_data, list(Y = Y, N = N, M = M,
                             n_sample = n_sample, n_prior = n_prior,
                             n_evolve_meanlog = n_evolve_meanlog,
                             n_evolve_sdlog = n_evolve_sdlog,
                             pi0 = pi_prior, Ygaps = as.array(gaps))))
  }
}
