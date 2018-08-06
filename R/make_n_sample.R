#' @export

make_n_sample <- function (target_ls) {
  Y <- length(target_ls)
  M <- estsubpop::get_max_margins(target_ls)
  n_sample <- array(0, dim = list(Y, M),
                    dimnames = list(period = 1:Y, margin = 1:M))
  for (y in 1:Y) {
    for (m in seq_along(target_ls[[y]])) {
      n_sample[y, m] <- sum(target_ls[[y]][[m]]$Freq)
    }
  }
  return(n_sample)
}
