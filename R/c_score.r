#' Estimate a conflict score for a single subject
#'
#' @param c_nmst A vector containing the parameters for from an ex-gaussian distribution, c(n, mu, sigma, tau), where n is the number of trials.
#' @param nc_nmst A vector containing the parameters for sampling from an ex-gaussian distribution, c(n, mu, sigma, tau), where n is the number of trials.
#' @return A simulated conflict score for one subject
#' @details This function creates a simulated conflict score,  defined as a mean reaction time for conflict trials minus the mean reaction time for non-conflict trials.
#'
#' Mean RTs for each each trial type (conflict vs. no conflict) are simulated using the rexgauss function from the retimes package. This allows any number of samples to be drawn from ex-gaussian distributions for each trial type.
#'
#' For conflict trials, the c_nmst parameter defines the number of trials (samples), and the mu, sigma, and tau parameters. For no-conflict trials, nc_nmst parameter defines the number of trials (samples), and the mu, sigma, and tau parameters.
#'
#' The function samples the programmed number of samples from each distribution, computes the mean fro each trial type, then returns the difference between the mean conflict RT and the mean no-conflict RT. The assumed direction of the difference is conflict - no conflict, so positive differences reflect slower RTs for conflict than no conflict items.
#'
#' This function is a wrapper to rexgauss, and is a helper function for computing simulated power analyses of conflict designs.
#'
#' The example below simulates a single conflict score for a subject who completes 10 conflict and 10 no conflict trials, with an assumed difference between the means of 100 ms.
#'
#' @examples
#' c_score(c_nmst= c(n=10, mu=600, sigma=100, tau=100),
#'         nc_nmst= c(n=10, mu=500, sigma=100, tau=100))
#' @export

c_score <- function(c_nmst = c(n,mu,sigma,tau),
                    nc_nmst = c(n,mu,sigma,tau)){

  conflict <- rexgauss(n=c_nmst[1],
                                mu=c_nmst[2],
                                sigma=c_nmst[3],
                                tau =c_nmst[4],
                                positive = TRUE)

  no_conflict <- rexgauss(n=nc_nmst[1],
                                   mu=nc_nmst[2],
                                   sigma=nc_nmst[3],
                                   tau =nc_nmst[4],
                                   positive = TRUE)

  return(mean(conflict) - mean(no_conflict))
}
