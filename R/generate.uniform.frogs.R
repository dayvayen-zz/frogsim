#' Generate a network of simfrogs
#' This function calls several other functions.
#'
#' @param n integer number of frogs
#' @param ratio numeric sex ratio of femlaes (percentage, 0-1)
#' @param pondsize numeric length of pond edges (meters)
#' @param fixed logical. set to T if fixed positions are desired
#' @param seed.x numeric seed for when fixed == T
#' @param seed.y numeric seed for when fixed == T
#' @param buffer logical whether you want the frogs to be on the periphery or randomly distributed.
#' @param temp numeric ambient air temperature (Celsius)
#' @param noiselevel numeric ambient noise level (dB re 20 microPa)
#' @return network object of frogs from all above parameters
#' @seealso \code{\link{uniform.frog.sex.ratio.setup}}, \code{\link{matrixify}}, \code{\link{make.network}}. This function is dependent on the previous functions.
#' @export
#' @examples
#' generate.uniform.frogs(30, .6, 30, 10, 28, F, 500, 500)

generate.uniform.frogs <- function(n, ratio, pondsize, fixed, seed.x, seed.y, buffer, temp, noiselevel) {
  tibs <- uniform.frog.sex.ratio.setup(n, ratio, pondsize, fixed, seed.x, seed.y, buffer)
  adj.matrix <- matrixify(tibs, noiselevel)
  g <- make.network(tibs, adj.matrix, temp, noiselevel)
  network::set.vertex.attribute(g, "sex", tibs$sex)
  network::set.vertex.attribute(g, "x.coord", tibs$x)
  network::set.vertex.attribute(g, "y.coord", tibs$y)
  network::set.network.attribute(g, "sex ratio", ratio)
  network::set.network.attribute(g, "pond size", pondsize)
  g
}
