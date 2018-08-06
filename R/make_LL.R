#' @export

make_LL <- function (target_ls) {
  Y <- length(target_ls)
  M <- estsubpop::get_max_margins(target_ls)
  LL <- array(0, dim = list(Y, M), dimnames = list(period = 1:Y, margin = 1:M))
  for (y in 1:Y) {
    for (m in seq_along(target_ls[[y]])) {
      LL[y, m] <- nrow(target_ls[[y]][[m]]) # number of cells in margin
    }
  }
  return(LL)
}
