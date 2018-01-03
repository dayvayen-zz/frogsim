#' Generate a bunch of input data to make a bunch of simfrog networks.
#'
#' @param n integer number of frogs (defaults to 30)
#' @param ratio numeric sex ratio of femlaes (percentage, 0-1; defaults to sequence from .1 to .9 by .1)
#' @param pondsize numeric length of pond edges (meters, defaults to 30)
#' @param temp numeric ambient air temperature (Celsius, defaults to 10)
#' @param noiselevel numeric ambient noise level (dB re 20 microPa, defaults to 28)
#' @param fixed logical. set to T if fixed positions are desired (defaults to F)
#' @param seed.x numeric seed for when fixed == T (defaults to 500)
#' @param seed.y numeric seed for when fixed == T (defaults to 600)
#' @param buffer logical whether you want the frogs to be on the periphery or randomly distributed.
#' @param replicate numeric how many times you want each network to generate
#' @return list of input data
#' @export
#' @examples
#' generate.input.data()

generate.input.data <- function(n = 30,
                                ratio = (seq(from=.1, to = .9, by = 0.1)),
                                pondsize = 30,
                                fixed = F,
                                seed.x = 500,
                                seed.y = 600,
                                buffer = T,
                                temp = 10,
                                noiselevel = 28,
                                replicate = 1) {
  input.data <- expand.grid(n, ratio, pondsize, fixed, seed.x, seed.y, buffer, temp, noiselevel)
  names(input.data) <- c("n", "ratio", "pondsize", "fixed", "seed.x", "seed.y", "buffer", "temp", "noiselevel")
  input.data <- mefa:::rep.data.frame(input.data, times = replicate)
  input.data.list <- split(input.data, seq(nrow(input.data)))
  input.data.list
}
