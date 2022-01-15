#' RD graph (outcome vs. running)
#'
#' @param A dataframe, where "Y" is the outcome, "D" is running var., W is treatment var.
#' @param c RD cutoff
#' @export
rd_graph <- function(dt, c, y_min_lim, y_max_lim) {
  mod <- rdrobust::rdrobust(y=dt$Y, x=dt$D, c=c, masspoints = "off")

  tau <- round(mod$Estimate[1], digits = 2)
  tau_se <-  round(mod$Estimate[3], digits = 2)

  # setup for graph
  title = bquote(plain(Estimated)~plain(ATE):~hat(tau) == .(tau) (.(tau_se)))

  dt.1.means <- dt[dt$W == 1,] %>% dplyr::group_by(D) %>% dplyr::summarise(Y = mean(Y))
  dt.0.means <- dt[dt$W == 0,] %>% dplyr::group_by(D) %>% dplyr::summarise(Y = mean(Y))

  ggplot2::ggplot() +
    ggplot2::geom_point(data = dt.0.means, mapping = aes(x = D, y = Y, color = "C"), alpha = .75) +
    ggplot2::geom_point(data = dt.1.means, mapping = aes(x = D, y = Y, color = "T"), alpha = .75) +
    ggplot2::geom_smooth(data = dt.0.means, mapping = aes(x = D, y = Y, color = "C"), method = "loess", se = F, size = 1.5) +
    ggplot2::geom_smooth(data = dt.1.means, mapping = aes(x = D, y = Y, color = "T"), method = "loess", se = F, size = 1.5) +
    ggplot2::geom_vline(xintercept = c, linetype = "dashed") +
    ggplot2::scale_color_manual(breaks = c("T", "C"), labels = c("Treatment", "Non-Treatment"), values = c(blue, orange), name = NULL) +
    ggplot2::theme(legend.position = c(0.25, 0.85), legend.title = element_blank()) +
    ggplot2::labs(y = "Outcome Variable", x = "Running Variable",
         subtitle = title) +
    ggplot2::scale_y_continuous(limits = c(y_min_lim, y_max_lim))
}


#' Tau vs Alpha sensitivity analysis plot
#'
#' @param alpha_grid  a grid to loop over alpha (if not provided will create)
#' @param tau_star the estimated treatment effect
#' @param tau_star_se the estimated treatment effect standard errors
#' @param ci_sig confidence interval significance level for rejection of H0: tau= 0 (default is 5%)
#' @export
alpha_graph <- function(alpha_grid = NULL, tau_star, tau_star_se, ci_sig = 0.05) {

  # run function to calculate tau by alpha and tau_star
  alpha_sens <- alpha_loop(alpha_grid, tau_star, tau_star_se, ci_sig)

  # keep maximum alpha that does not reject null
  max_alpha <- alpha_sens %>%
    dplyr::filter(reject_null == 0) %>%
    dplyr::select(alpha) %>%
    max()

  # setup for graph
  title = bquote(plain(Max.)~plain(rej.)~plain(of)~plain(null):~alpha == .(max_alpha))
  dt.0 <- alpha_sens %>% dplyr::filter(reject_null == 0)
  dt.1 <- alpha_sens %>% dplyr::filter(reject_null == 1)

  # plot tau as function of alpha
  # (color nonrejection area as red and rejection area as green)
  g <- alpha_sens %>%
    ggplot2::ggplot(mapping = aes(x = alpha, y = tau)) +
    ggplot2::geom_line() +
    ggplot2::geom_ribbon(data=dt.0, mapping = aes(ymin=ci_l,ymax=ci_h, fill="N"), alpha=0.5) +
    ggplot2::geom_ribbon(data=dt.1, mapping = aes(ymin=ci_l,ymax=ci_h, fill="Y"), alpha=0.5) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed") +
    ggplot2::scale_fill_manual(breaks = c("Y", "N"), labels = c("Reject Null", "Don't Reject Null"), values = c("green", "red"), name = NULL) +
    ggplot2::theme(legend.position = c(0.25, 0.85), legend.title = element_blank()) +
    ggplot2::scale_y_continuous(expand = c(0, 0)) +
    ggplot2::scale_x_continuous(expand = c(0, 0)) +
    ggplot2::labs(y = "Weighted Treatment Effect", x = expression(alpha),
         subtitle = title)
  return(g)
}
