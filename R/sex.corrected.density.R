#' Sex-corrected density measure for only finding the male-female connections.
#'
#' @param g network object
#' @return numeric sex-corrected density
#' @export

sex.corrected.density <- function(g) {
  edgecount <- network::network.edgecount(g)
  test <-network::get.vertex.attribute(g, "sex") == "f"
  m.f.combo <- table(test)[1] * table(test)[2]
  sex.corrected.density <- edgecount/m.f.combo
  names(sex.corrected.density) <- NULL
  sex.corrected.density
}
