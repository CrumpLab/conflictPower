#' Generate random deviate from ex-gaussian distribution
#'
#' @param n integer, number of deviates to generate
#' @param mu integer, mean of normal
#' @param sigma integer, sd of normal
#' @param tau integer, tail parameter
#' @param positive logical, force values to be positive
#'
#' @return a vector of random deviates from an ex-gaussian distribution
#' @details This function was originally part of the retimes package, authored by Davide Massidda, which was removed from CRAN on 2021-01-27.
#' @export
#'
#' @examples
#' rexgauss(n=10,mu=0,sigma=1,tau=1)
#'
rexgauss <- function(n, mu=0, sigma=1, tau=1, positive=TRUE) {
  if(positive) {
    while(1) {
      x <- rnorm(n,mu,sigma)+rexp(n,1/tau)
      if(sum(x>0)==n) break
    }
  } else
    x <- rnorm(n,mu,sigma)+rexp(n,1/tau)
  return(x)
}
