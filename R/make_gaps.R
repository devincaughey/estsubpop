#' @export

make_gaps <- function (yrs_to_est) {
  if (!is.integer(yrs_to_est)) {
    cat("\nWarning: Coercing", print(head(yrs_to_est)), "... to integer\n")
    yrs_to_est <- as.integer(yrs_to_est)
  }
  gaps <- c(0, yrs_to_est[-1] - yrs_to_est[-length(yrs_to_est)])
  as.array(gaps)
}
