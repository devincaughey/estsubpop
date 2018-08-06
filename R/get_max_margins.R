#' @export

get_max_margins <- function (target_ls) {
  as.numeric(max(unlist(plyr::llply(target_ls, length))))
}
