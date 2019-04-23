#' Estimate power for conflict effect using monte-carlo simulation, return table and plot
#'
#' @param subjects A vector for the numbers of subjects across simulated experiments
#' @param differences A vector for the mean differences between conflict and no-conflict trials across simulations
#' @param c_nmst A vector containing the parameters for an ex-gaussian distribution, c(n, mu, sigma, tau), where n is the number of trials.
#' @param nc_nmst A vector containing the parameters for an ex-gaussian distribution, c(n, mu, sigma, tau), where n is the number of trials.
#' @param num_sims A number, simulations to run
#' @param alpha A number, alpha criterion
#' @return A list, $power_table contains a table with power estimates as a function of number of subjects and mean differences, $power_curve contains a graph (using ggplot2) showing power as a function of subjects and mean differences
#' @details  This function is an extension to c_power_fast, that allows multiple estimates of power for a vector specifying numbers of subjects and mean differenes.
#'
#' This function uses monte-carlo simulation to determine statistical power associated for detecting a conflict effect, and includes paramaters for number of subjects in the experiment, number of trials in each condition (conflict vs. no-conflict), and paramaters (mu,sigma,tau) for each reaction time distribution.
#'
#' For every simulated experiment, a one sample t-test  (two-tailed) is computed, and the p-value is saved. Power is the proportion of simulated experiments that return p-values less than the defined alpha criterion.
#'
#' @examples
#' test <- c_power_table(subjects = c(10,20,30),
#'                 differences = c(10,20,30),
#'                 c_nmst = c(50,732.4,80.7,157.5),
#'                 nc_nmst = c(50,625.4,68.6,166.3),
#'                 num_sims = 100,
#'                 alpha = .05)
#' test$power_table
#' test$power_curve
#' @export


c_power_table <- function(subjects,
                          differences,
                          c_nmst,
                          nc_nmst,
                          num_sims = 100,
                          alpha=.05){

  require(ggplot2)

  pwr_df <- data.frame()
  for(j in differences){
    c_params <- c_nmst
    c_params[2] <- nc_nmst[2]+j
    for(i in subjects){
      power <-c_power_fast(subjects=i,
                           c_params,
                           nc_nmst,
                           num_sims,
                           alpha = .05)
      t_df <- data.frame(subjects = i,
                         differences = j,
                         power)
      pwr_df <- rbind(pwr_df,t_df)
    }
  }

  pwr_df$differences<-as.factor(pwr_df$differences)

  g_pwr_plt <- ggplot2::ggplot(pwr_df, aes(x=subjects,y=power, group=differences,
                                  color=differences))+
    geom_line()+
    geom_point()+
    theme_classic(base_size=12)+
    scale_y_continuous(breaks=seq(0,1,.1), limits=c(0,1))

  return(list(power_table = pwr_df,
              power_curve = g_pwr_plt))
}

