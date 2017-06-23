#' Generate a network of simfrogs
#' This function calls several other functions.
#'
#' @param n integer number of frogs
#' @param ratio numeric sex ratio of femlaes (percentage, 0-1)
#' @param pondsize numeric length of pond edges (meters)
#' @param temp numeric ambient air temperature (Celsius)
#' @param noiselevel numeric ambient noise level (dB re 20 microPa)
#' @param fixed logical. set to T if fixed positions are desired
#' @param seed.x numeric seed for when fixed == T
#' @param seed.y numeric seed for when fixed == T
#' @return network object of frogs from all above parameters
#' @seealso \code{\link{uniform.frog.sex.ratio.setup}}, \code{\link{matrixify}}, \code{\link{make.network}}. This function is dependent on the previous functions.
#' @export
#' @examples
#' generate.uniform.frogs(30, .6, 30, 10, 28, F, 500, 500)

generate.uniform.frogs <- function(n, ratio, pondsize, temp, noiselevel, fixed, seed.x, seed.y) {
  df <- uniform.frog.sex.ratio.setup(n, ratio, pondsize, fixed, seed.x, seed.y)
  adj.matrix <- matrixify(df, noiselevel)
  g <- make.network(df, adj.matrix, temp, noiselevel)
  g %v% "sex" <- df$sex
  g %v% "x.coord" <- df$x
  g %v% "y.coord" <- df$y
  network::set.network.attribute(g, "sex ratio", ratio)
  g
}
