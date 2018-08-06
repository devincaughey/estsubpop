paste_formula <- function (xnam, ynam = "") {
  as.formula(paste(paste(ynam, " ~ "), paste(xnam, collapse = "+")))
}
