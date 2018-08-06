#' @export

order_levels <- function (x) {
  levels <- levels(as.factor(x))
  non <- grepl("Non-", levels)
  factor(x, levels = c(levels[non], levels[!non]))
}
