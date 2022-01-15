#' A cosemtic function to organize three graphs in one row
#'
#' @param g1 Left graph
#' @param g2 Center graph
#' @param g3 Right graph
#' @param title1 Title for Left graph
#' @param title2 Title for Center graph
#' @param title3 Title for Right graph
#'
#' @return A single graph
#' @export
threeway_graph <- function(g1,g2,g3,title1,title2,title3) {

  g <- ggarrange(g1 + theme(plot.margin = margin(r=5,b=15,t=5,l=5)) +
                   labs(title = title1) + theme(legend.position = "none"),
                 g2 + theme(axis.title.y = element_blank(),
                            plot.margin = margin(l=30,b=15, r=10, t=5)) +
                   labs(title = title2) + theme(legend.position = "none"),
                 g3 + theme(axis.title.y = element_blank(),
                            plot.margin = margin(r=20,b=15,l=20,t=5)) +
                   labs(title = title3),
                 nrow = 1, ncol = 3
  )
  return(g)
}

#' A cosemtic function to organize three graphs in one row, without a title
#'
#' @param g1 Left graph
#' @param g2 Center graph
#' @param g3 Right graph
#'
#' @return A single graph
#' @export
threeway_graph_non_title <- function(g1,g2,g3) {

  g <- ggarrange(g1 + theme(plot.margin = margin(r=5,b=15,t=5,l=5)) +
                   theme(legend.position = "none"),
                 g2 + theme(axis.title.y = element_blank(),
                            plot.margin = margin(l=30,b=15, r=10, t=5)) +
                   theme(legend.position = "none"),
                 g3 + theme(axis.title.y = element_blank(),
                            plot.margin = margin(r=20,b=15,l=20,t=5)),
                 nrow = 1, ncol = 3
  )
  return(g)
}

#' Cosmetic function for autmatic latex table saving using kableExtra
#'
#' @param data Table dataframe
#'
#' @return Latex table output
#' @export
summary_tab <- function(data) {
  num_cols <- ncol(data)-1
  tab <- data %>%
    dplyr::mutate(tau_star = round(tau_star, digits = 2),
           tau_alpha_max = round(tau_alpha_max, digits = 2),
           tau_star_se = round(tau_star_se, digits = 2),
           p_value = round(p_value, digits = 3)) %>%
    dplyr::relocate(data) %>%
    kableExtra::kbl(caption = "Simulation Summary", booktabs = T, format = "latex",
        align = c("l", rep("c",num_cols))) %>%
    kableExtra::kable_styling(position = "center")
  return(tab)
}
