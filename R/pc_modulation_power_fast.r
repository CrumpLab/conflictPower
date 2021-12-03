#' Estimate power for differences between proportion conflict effects using monte-carlo simulation
#'
#' @param subjects A number for the number of subjects in simulated experiment
#' @param design A string set to "paired" or "between"
#' @param A_mc_c_nmst A vector for mostly conflict (mc), conflict trials (c) containing the parameters for an ex-gaussian distribution, c(n, mu, sigma, tau), where n is the number of trials.
#' @param A_mc_nc_nmst A vector for mostly conflict (mc), conflict trials (c) containing the parameters for an ex-gaussian distribution, c(n, mu, sigma, tau), where n is the number of trials.
#' @param A_mnc_c_nmst A vector for mostly no-conflict (mnc), conflict trials (c) containing the parameters for an ex-gaussian distribution, c(n, mu, sigma, tau), where n is the number of trials.
#' @param A_mnc_nc_nmst A vector for mostly no-conflict (mnc), conflict trials (c) containing the parameters for an ex-gaussian distribution, c(n, mu, sigma, tau), where n is the number of trials.
#' @param B_mc_c_nmst A vector for mostly conflict (mc), conflict trials (c) containing the parameters for an ex-gaussian distribution, c(n, mu, sigma, tau), where n is the number of trials.
#' @param B_mc_nc_nmst A vector for mostly conflict (mc), conflict trials (c) containing the parameters for an ex-gaussian distribution, c(n, mu, sigma, tau), where n is the number of trials.
#' @param B_mnc_c_nmst A vector for mostly no-conflict (mnc), conflict trials (c) containing the parameters for an ex-gaussian distribution, c(n, mu, sigma, tau), where n is the number of trials.
#' @param B_mnc_nc_nmst A vector for mostly no-conflict (mnc), conflict trials (c) containing the parameters for an ex-gaussian distribution, c(n, mu, sigma, tau), where n is the number of trials.
#' @param num_sims A number, simulations to run
#' @param alpha A number, alpha criterion
#' @return A number, power: the proportion of simulated experiments that returned a p-value less than the alpha criterion for the conflict effect
#' @details  This function uses monte-carlo simulation to determine statistical power associated for detecting a modulation to a proportion conflict effect, specifically a difference between two proportion conflict effects.
#'
#' Power is the proportion of simulated experiments that return p-values less than the defined alpha criterion.
#'
#' @examples
#' pc_modulation_power_fast(subjects=20,
#'   design ="paired",
#'   A_mc_c_nmst = c(80,550,100,100),
#'   A_mc_nc_nmst = c(20,500,100,100),
#'   A_mnc_c_nmst = c(20,570,100,100),
#'   A_mnc_nc_nmst = c(80,500,100,100),
#'   B_mc_c_nmst = c(80,530,100,100),
#'   B_mc_nc_nmst = c(20,500,100,100),
#'   B_mnc_c_nmst = c(20,570,100,100),
#'   B_mnc_nc_nmst = c(80,500,100,100),
#'   num_sims = 1000,
#'   alpha = .05)
#' @export

pc_modulation_power_fast <- function(subjects,
                                     design,
                                     A_mc_c_nmst,
                                     A_mc_nc_nmst,
                                     A_mnc_c_nmst,
                                     A_mnc_nc_nmst,
                                     B_mc_c_nmst,
                                     B_mc_nc_nmst,
                                     B_mnc_c_nmst,
                                     B_mnc_nc_nmst,
                                     num_sims=1000,
                                     alpha=.05) {

  p_values <- c()
  for(i in 1: num_sims){
    A_mc_c_mat <- matrix(rexgauss(subjects*A_mc_c_nmst[1],
                                           A_mc_c_nmst[2],A_mc_c_nmst[3],A_mc_c_nmst[4],
                                         positive=TRUE),
                       nrow=subjects,ncol=A_mc_c_nmst[1])
    A_mc_nc_mat <- matrix(rexgauss(subjects*A_mc_nc_nmst[1],
                                            A_mc_nc_nmst[2],A_mc_nc_nmst[3],A_mc_nc_nmst[4],
                                          positive=TRUE),
                        nrow=subjects,ncol=A_mc_nc_nmst[1])
    A_mnc_c_mat <- matrix(rexgauss(subjects*A_mnc_c_nmst[1],
                                            A_mnc_c_nmst[2],A_mnc_c_nmst[3],A_mnc_c_nmst[4],
                                          positive=TRUE),
                        nrow=subjects,ncol=A_mnc_c_nmst[1])
    A_mnc_nc_mat <- matrix(rexgauss(subjects*A_mnc_nc_nmst[1],
                                             A_mnc_nc_nmst[2],A_mnc_nc_nmst[3],A_mnc_nc_nmst[4],
                                           positive=TRUE),
                         nrow=subjects,ncol=A_mnc_nc_nmst[1])
    A_diff_mat <- (rowMeans(A_mnc_c_mat)-rowMeans(A_mnc_nc_mat))-(rowMeans(A_mc_c_mat)-rowMeans(A_mc_nc_mat))

    ## get Bs

    B_mc_c_mat <- matrix(rexgauss(subjects*B_mc_c_nmst[1],
                                           B_mc_c_nmst[2],B_mc_c_nmst[3],B_mc_c_nmst[4],
                                           positive=TRUE),
                         nrow=subjects,ncol=B_mc_c_nmst[1])
    B_mc_nc_mat <- matrix(rexgauss(subjects*B_mc_nc_nmst[1],
                                            B_mc_nc_nmst[2],B_mc_nc_nmst[3],B_mc_nc_nmst[4],
                                            positive=TRUE),
                          nrow=subjects,ncol=B_mc_nc_nmst[1])
    B_mnc_c_mat <- matrix(rexgauss(subjects*B_mnc_c_nmst[1],
                                            B_mnc_c_nmst[2],B_mnc_c_nmst[3],B_mnc_c_nmst[4],
                                            positive=TRUE),
                          nrow=subjects,ncol=B_mnc_c_nmst[1])
    B_mnc_nc_mat <- matrix(rexgauss(subjects*B_mnc_nc_nmst[1],
                                             B_mnc_nc_nmst[2],B_mnc_nc_nmst[3],B_mnc_nc_nmst[4],
                                             positive=TRUE),
                           nrow=subjects,ncol=B_mnc_nc_nmst[1])
    B_diff_mat <- (rowMeans(B_mnc_c_mat)-rowMeans(B_mnc_nc_mat))-(rowMeans(B_mc_c_mat)-rowMeans(B_mc_nc_mat))

    if(design == "paired"){
      p_values[i] <- t.test(A_diff_mat,B_diff_mat, paired=TRUE)$p.value
    } else if (design == "between"){
      p_values[i] <- t.test(A_diff_mat,B_diff_mat, var.equal=TRUE)$p.value
    }


  }

  power <- length(p_values[p_values < alpha])/num_sims
  return(power)

}

