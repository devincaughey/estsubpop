#' @export

make_XX <- function (target_ls) {
  levels_ls <- estsubpop::get_var_levels(target_ls)
  init_df <- data.frame(matrix(nrow = 1, ncol = 0))
  for (v in seq_along(levels_ls)) {
    init_df[[names(levels_ls)[v]]] <- factor(NA, levels = levels_ls[[v]])
  }
  as.data.frame(xtabs(estsubpop::make_formula(names(init_df)), init_df))
}
