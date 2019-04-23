#' Estimate power for conflict effect using monte-carlo simulation, faster version
#'
#' @param subjects A number for the number of subjects in simulated experiment
#' @param c_nmst A vector containing the parameters for an ex-gaussian distribution, c(n, mu, sigma, tau), where n is the number of trials.
#' @param nc_nmst A vector containing the parameters for an ex-gaussian distribution, c(n, mu, sigma, tau), where n is the number of trials.
#' @param num_sims A number, simulations to run
#' @param alpha A number, alpha criterion
#' @return A number, power: the proportion of simulated experiments that returned a p-value less than the alpha criterion for the conflict effect
#' @details  This function is a faster version of c_power.
#'
#' This function uses monte-carlo simulation to determine statistical power associated for detecting a conflict effect, and includes paramaters for number of subjects in the experiment, number of trials in each condition (conflict vs. no-conflict), and paramaters (mu,sigma,tau) for each reaction time distribution.
#'
#' For every simulated experiment, a one sample t-test  (two-tailed) is computed, and the p-value is saved. Power is the proportion of simulated experiments that return p-values less than the defined alpha criterion.
#'
#' @examples
#' c_power_fast(subjects=10,
#'   c_nmst = c(20,550,100,100),
#'   nc_nmst = c(20,500,100,100),
#'   num_sims = 1000,
#'   alpha = .05)
#' @export


c_power_fast <- function( subjects,
                     c_nmst,
                     nc_nmst,
                     num_sims = 10000,
                     alpha = .05) {
  p_values <- c()
  for(i in 1: num_sims){
    c_mat <- matrix(retimes::rexgauss(subjects*c_nmst[1],
                                      c_nmst[2],c_nmst[3],c_nmst[4],
                                      positive=TRUE),
                    nrow=subjects,ncol=c_nmst[1])
    nc_mat <- matrix(retimes::rexgauss(subjects*nc_nmst[1],
                                       nc_nmst[2],nc_nmst[3],nc_nmst[4],
                                       positive=TRUE),
                     nrow=subjects,ncol=nc_nmst[1])
    diff_mat <- rowMeans(c_mat)-rowMeans(nc_mat)
    p_values[i] <- t.test(diff_mat)$p.value
  }

  power <- length(p_values[p_values < alpha])/num_sims
  return(power)

}

