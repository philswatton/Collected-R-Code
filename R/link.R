
inv_logit <- function(x) {
  p <- 1 / (1 + exp(-x))
  return(p)
}
