#' @export

get_names_from_list <- function (ls, except = "Freq") {
  x <- plyr::llply(ls, function (x) {
    plyr::llply(x, names)
  })
  setdiff(unique(unlist(x)), except)
}
