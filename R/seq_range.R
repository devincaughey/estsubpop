seq_range <- function (x) {
  if (is.factor(x)) {
    x <- as.character(x)
  }
  seq.int(min(as.integer(x)), max(as.integer(x)))
}
