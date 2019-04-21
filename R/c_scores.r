#' Estimate conflict scores for n subjects
#'
#' @param subjects A number for the number conflict scores to generate
#' @param c_nmst A vector containing the parameters for from an ex-gaussian distribution, c(n, mu, sigma, tau), where n is the number of trials.
#' @param nc_nmst A vector containing the parameters for sampling from an ex-gaussian distribution, c(n, mu, sigma, tau), where n is the number of trials.
#' @return A simulated conflict score for one subject
#' @details This function creates simulated conflict scores for n number of subjects. This is a wrapper for c_score, which creates a single conflict score.
#'
#' @examples
#' c_scores(subjects=50,
#'         c_nmst= c(n=10, mu=600, sigma=100, tau=100),
#'         nc_nmst= c(n=10, mu=500, sigma=100, tau=100))
#' @export


c_scores <- function(subjects,
                     c_nmst = c(n,mu,sigma,tau),
                     nc_nmst = c(n,mu,sigma,tau)) {

  conflict_scores <- replicate(subjects,
                               c_score(
                                 c_nmst,
                                 nc_nmst
                               )
  )

  return(conflict_scores)
}

