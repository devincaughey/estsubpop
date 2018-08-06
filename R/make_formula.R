#' @export

make_formula <- function (xvars = NULL, yvars = NULL) {
  xform <- paste(xvars, collapse = "+")
  yform <- paste(yvars, collapse = "+")
  as.formula(paste(yform, "~", xform))
}
