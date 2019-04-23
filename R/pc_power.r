#' Estimate power for proportion conflict effect using monte-carlo simulation
#'
#' @param subjects A number for the number of subjects in simulated experiment
#' @param mc_c_nmst A vector for mostly conflict (mc), conflict trials (c) containing the parameters for an ex-gaussian distribution, c(n, mu, sigma, tau), where n is the number of trials.
#' @param mc_nc_nmst A vector for mostly conflict (mc), conflict trials (c) containing the parameters for an ex-gaussian distribution, c(n, mu, sigma, tau), where n is the number of trials.
#' @param mnc_c_nmst A vector for mostly no-conflict (mnc), conflict trials (c) containing the parameters for an ex-gaussian distribution, c(n, mu, sigma, tau), where n is the number of trials.
#' @param mnc_nc_nmst A vector for mostly no-conflict (mnc), conflict trials (c) containing the parameters for an ex-gaussian distribution, c(n, mu, sigma, tau), where n is the number of trials.
#' @param num_sims A number, simulations to run
#' @param alpha A number, alpha criterion
#' @return A number, power: the proportion of simulated experiments that returned a p-value less than the alpha criterion for the conflict effect
#' @details  This function uses monte-carlo simulation to determine statistical power associated for detecting a proportion conflict effect, specifically a difference between two conflict effect, typically based on a manipulation of the proportion of conflict and no-conflict trials.
#'
#' For every simulated experiment, a one sample t-test  (two-tailed) is computed, and the p-value is saved. Power is the proportion of simulated experiments that return p-values less than the defined alpha criterion.
#'
#' @examples
#' pc_power(subjects=10,
#'   mc_c_nmst = c(80,550,100,100),
#'   mc_nc_nmst = c(20,500,100,100),
#'   mnc_c_nmst = c(20,570,100,100),
#'   mnc_nc_nmst = c(80,500,100,100),
#'   num_sims = 1000,
#'   alpha = .05)
#' @export

pc_power <- function(subjects,
                            mc_c_nmst,
                            mc_nc_nmst,
                            mnc_c_nmst,
                            mnc_nc_nmst,
                            num_sims,
                            alpha) {

  p_values <- replicate(num_sims,
                        t.test(c_scores(subjects,
                                        mc_c_nmst,
                                        mc_nc_nmst)-
                                 c_scores(subjects,
                                          mnc_c_nmst,
                                          mnc_nc_nmst))$p.value)

  power <- length(p_values[p_values < alpha])/num_sims
  return(power)

}

