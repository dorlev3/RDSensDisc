#' Alpha grid search
#'
#' @param alpha_grid a grid to loop over alpha (if not provided will create with 101 values)
#' @param tau_star the estimated treatment effect
#' @param tau_star_se the estimated treatment effect standard errors
#' @param ci_sig confidence interval signifcance level for rejection of H0: tau= 0 (default is 5%)
#'
#' @return a dataframe containing alpha levels and corresponding taus and rejections of null hypothesis
#' @export
alpha_loop <- function(alpha_grid = NULL, tau_star, tau_star_se, ci_sig = 0.05) {

  # set up alpha grid
  if(is.null(alpha_grid)) {
    alpha_grid = seq(from = 0,to = 1,by = 0.01)
  }

  df = data.frame(alpha = alpha_grid) %>%
    dplyr::mutate(
      # calculate tau as function of tau_star and alpha
      tau = alpha * tau_star,
      # t statistic for the null hypothesis
      tau_se = tau_star_se,
      t_stat = tau/tau_se,
      # rejection and confidence intervals
      reject_null = ifelse(t_stat >= qnorm(1-ci_sig/2), 1, 0),
      ci_l = tau - tau_se * qnorm(1-ci_sig/2),
      ci_h = tau + tau_se * qnorm(1-ci_sig/2)
    )

  return(df)
}

#' Conservative Continuity Hypothesis Testing
#'
#' This function tests the hypothesis of treatment effect equalling maximum ``continuous'' treatment effect
#'
#' @param alpha_grid a grid to loop over alpha (if not provided will create with 101 values)
#' @param tau_star the estimated treatment effect
#' @param tau_star_se the estimated treatment effect standard errors
#' @param ci_sig confidence interval signifcance level for rejection of H0: tau= 0 (default is 5%)
#'
#' @return A list containing alpha-max, tau(alpha-max) p-value of rejection of the conservative H0 was rejected
#' @export
disc_test <- function(alpha_grid = NULL, tau_star, tau_star_se, ci_sig = 0.05) {

  # run function to calculate tau by alpha and tau_star
  alpha_sens <- alpha_loop(alpha_grid, tau_star, tau_star_se, ci_sig)

  # keep maximum alpha that does not reject null
  max_alpha <- alpha_sens %>%
    dplyr::filter(reject_null == 0) %>%
    dplyr::select(alpha) %>%
    max()

  max_tau <- alpha_sens %>%
    dplyr::filter(alpha == max_alpha) %>%
    dplyr::select(tau) %>%
    unlist()

  t_stat <- abs((tau_star - max_tau) / tau_star_se)
  p_value <- (1-pnorm(t_stat))*2

  return(list("tau_star" = tau_star,
              "tau_star_se" = tau_star_se,
              "alpha_max" = max_alpha,
              "tau_alpha_max" = max_tau,
              "p_value" = p_value))
}
