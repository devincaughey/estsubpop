#' @export

melt_pi <- function (pi_array, yr_names, xtab) {
  total_its <- dim(pi_array)[1] * dim(pi_array)[2]
  pi_melt <- reshape2::melt(pi_array)
  pi_melt$yr__ <-
    as.integer(gsub("pi\\[([0-9]*),[0-9]*\\]", "\\1", pi_melt$parameters))
  pi_melt$gp__ <-
    as.integer(gsub("pi\\[[0-9]*,([0-9]*)\\]", "\\1", pi_melt$parameters))
  pi_melt <- plyr::arrange(pi_melt, chains, iterations, yr__, gp__)
  xtab_yr <- dplyr::bind_rows(plyr::llply(seq_along(yr_names), function (i) xtab))
  xtab_yr$Period <- rep(yr_names, each = nrow(xtab))
  xtab_rep <- dplyr::bind_rows(plyr::llply(1:total_its, function (i) xtab_yr))
  data.frame(xtab_rep, pi_melt)
}
