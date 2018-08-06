#' @export

make_target_ls <- function (design_list, formulae_list, periods,
                            period_var = "YEAR") {
  out <- vector("list", length(periods))
  names(out) <- periods
  for (y in seq_along(periods)) {
    out[[y]] <- list()
    for (d in seq_along(design_list)) {
      for (f in seq_along(formulae_list[[d]])) {
        ds <- subset(design_list[[d]],
                     design_list[[d]]$variables[, period_var] == periods[y])
        df <- as.data.frame(survey::svytable(formulae_list[[d]][[f]], ds))
        df$Freq <- df$Freq / length(formulae_list[[d]]) #avoid double-counting
        if (sum(df$Freq) > 0) {
          out[[y]] <- c(out[[y]], list(df))
          names(out[[y]])[length(out[[y]])] <-
            paste(d, paste(formulae_list[[d]][[f]], collapse = ""), sep = " | ")
        }
      }
    }
  }
  return(out)
}
