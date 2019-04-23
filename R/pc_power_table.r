#' Estimate power for proportion conflict effect using monte-carlo simulation, return table and plot
#'
#' @param subjects A vector for the numbers of subjects across simulated experiments
#' @param differences A vector for the sizes of differences between conflict effects
#' @param base_conflict A number setting the size of the conflict effect for the mostly no conflict items.
#' @param mc_c_nmst A vector containing the parameters for an ex-gaussian distribution, c(n, mu, sigma, tau), where n is the number of trials.
#' @param mc_nc_nmst A vector containing the parameters for an ex-gaussian distribution, c(n, mu, sigma, tau), where n is the number of trials.
#' @param mnc_c_nmst A vector containing the parameters for an ex-gaussian distribution, c(n, mu, sigma, tau), where n is the number of trials.
#' @param mnc_nc_nmst A vector containing the parameters for an ex-gaussian distribution, c(n, mu, sigma, tau), where n is the number of trials.
#' @param num_sims A number, simulations to run
#' @param alpha A number, alpha criterion
#' @return A list, $power_table contains a table with power estimates as a function of number of subjects and mean differences, $power_curve contains a graph (using ggplot2) showing power as a function of subjects and mean differences
#' @details  This function is an extension to pc_power_fast, that allows multiple estimates of power for a vector specifying numbers of subjects and mean differences.
#'
#' This function uses monte-carlo simulation to determine statistical power associated for detecting a conflict effect, and includes paramaters for number of subjects in the experiment, number of trials in each condition (conflict vs. no-conflict), and paramaters (mu,sigma,tau) for each reaction time distribution.
#'
#' For every simulated experiment, a one sample t-test  (two-tailed) is computed, and the p-value is saved. Power is the proportion of simulated experiments that return p-values less than the defined alpha criterion.
#'
#' @examples
#' test <- pc_power_table(subjects = c(10,20,30),
#'                 differences = c(10,20,30),
#'                 mc_c_nmst = c(50,705,80.7,157.5),
#'                 mc_nc_nmst = c(50,625,68.6,166.3),
#'                 mnc_c_nmst = c(50,725,80.7,157.5),
#'                 mnc_nc_nmst = c(50,625,68.6,166.3),
#'                 num_sims = 100,
#'                 alpha = .05)
#' test$power_table
#' test$power_curve
#' @export


pc_power_table <- function(subjects,
                          differences,
                          base_conflict,
                          mc_c_nmst,
                          mc_nc_nmst,
                          mnc_c_nmst,
                          mnc_nc_nmst,
                          num_sims = 100,
                          alpha=.05){

  require(ggplot2)

  pwr_df <- data.frame()
  for(j in differences){
    mc_c_params <- mc_c_nmst
    mnc_c_params <- mnc_c_nmst

    mnc_c_params[2] <- mnc_nc_nmst[2]+base_conflict
    mc_c_params[2] <- mc_nc_nmst[2]+base_conflict-j

    for(i in subjects){
      power <-pc_power_fast(subjects=i,
                            mc_c_params,
                            mc_nc_nmst,
                            mnc_c_params,
                            mnc_nc_nmst,
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

