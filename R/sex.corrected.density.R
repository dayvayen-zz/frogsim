#' Sex-corrected density measure for only finding the male-female connections.
#'
#' @param g network object
#' @return numeric sex-corrected density
#' @export

sex.corrected.density <- function(g) {
  edgecount <- network.edgecount(g)
  m.f.combo <- count(g %v% "sex" == "f")$freq[1] * count(g %v% "sex" == "f")$freq[2]
  sex.corrected.density <- edgecount/m.f.combo
  sex.corrected.density
}
