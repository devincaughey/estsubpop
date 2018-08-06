#' @export

get_var_names <- function (ls, unlist = TRUE, except = "Freq") {
  x <- plyr::llply(ls, function (y) {
    unique(plyr::llply(y, function (z) setdiff(names(z), except)))
  })
  if (unlist) {
    return(unique(unlist(x)))
  } else {
    return(x)
  }
}
