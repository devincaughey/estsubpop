#' @export

make_design <- function (data, weights = ~Freq, ids = ~1, ...) {
  survey::svydesign(ids = ids, weights = weights, data = data, ...)
}
