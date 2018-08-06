#' @export

get_var_levels <- function (target_ls, except = "Freq") {
  levels_ls <- list()
  for (t in seq_along(target_ls)) {
    for (m in seq_along(target_ls[[t]])) {
      df <- target_ls[[t]][[m]]
      for (v in seq_along(df)) {
        if (!names(df)[v] %in% names(levels_ls) && !names(df)[v] %in% except) {
          stopifnot(is.factor(df[[v]]))
          levels_ls <- c(levels_ls, list(levels(df[[v]])))
          names(levels_ls)[length(levels_ls)] <- names(df)[v]
        }
      }
    }
  }
  return(levels_ls)
}
