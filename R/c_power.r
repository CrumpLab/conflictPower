#' Estimate power for conflict effect using monte-carlo simulation
#'
#' @param subjects A number for the number of subjects in simulated experiment
#' @param c_nmst A vector containing the parameters for an ex-gaussian distribution, c(n, mu, sigma, tau), where n is the number of trials.
#' @param nc_nmst A vector containing the parameters for an ex-gaussian distribution, c(n, mu, sigma, tau), where n is the number of trials.
#' @param num_sims A number, simulations to run
#' @param alpha A number, alpha criterion
#' @return A number, power: the proportion of simulated experiments that returned a p-value less than the alpha criterion for the conflict effect
#' @details  This function uses monte-carlo simulation to determine statistical power associated for detecting a conflict effect, and includes paramaters for number of subjects in the experiment, number of trials in each condition (conflict vs. no-conflict), and paramaters (mu,sigma,tau) for each reaction time distribution.
#'
#' For every simulated experiment, a one sample t-test  (two-tailed) is computed, and the p-value is saved. Power is the proportion of simulated experiments that return p-values less than the defined alpha criterion.
#'
#' @examples
#' c_power(subjects=10,
#'   c_nmst = c(20,500,100,100),
#'   nc_nmst = c(20,550,100,100),
#'   num_sims = 1000,
#'   alpha = .05)
#' @export


c_power <- function( subjects,
                     c_nmst,
                     nc_nmst,
                     num_sims = 1000,
                     alpha = .05) {

  p_values <- replicate(num_sims,
                        t.test(c_scores(subjects,
                                        c_nmst,
                                        nc_nmst))$p.value)

  power <- length(p_values[p_values < alpha])/num_sims
  return(power)

}

