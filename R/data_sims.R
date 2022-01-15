
#' Experiment Data Simulation
#' @noRd
data_exp <- function(N = 10000, a = 1, cut = 50, tau) {
  # seed so always same data given parameters
  set.seed(6)

  # generate treatment and outcome
  dt <- data.frame(id = 1:N) %>%
    mutate(
      # some covariate that determines the outcome
      X = rnorm(N, 0, 2),
      # uniform running variable
      D = floor(runif(N)*100),
      # treatment allocation mechanism
      W = 1*(D >= cut),
      # mean outcome function without treatment
      mu = a + X,
      # final outcome function with treatment effect and some error
      Y = mu + W*tau + rnorm(N, 0, sd = 2)
    )

  return(dt)
}

#' RD Linear Slopes Data Simulation
#' @noRd
data_levels <- function(N = 10000, a = 1, b = 0.05, cut = 50, tau) {
  # seed so always same data given parameters
  set.seed(6)

  # generate treatment and outcome
  dt <- data.frame(id = 1:N) %>%
    mutate(
      # some covariate that determines the outcome
      X = rnorm(N, 0, 2),
      # uniform running variable
      D = floor(runif(N)*100),
      # treatment allocation mechanism
      W = 1*(D >= cut),
      # mean outcome function without treatment
      mu = a + X + D*b,
      # final outcome function with treatment effect and some error
      Y = mu + W*tau + rnorm(N, 0, sd = 2)
    )
  return(dt)
}

#' RD Exponential Slopes Data Simulation
#' @noRd
data_slopes <- function(N = 10000, a = 1, b = 0.0005, cut = 50,
                        tau) {
  # seed so always same data given parameters
  set.seed(6)

  # generate treatment and outcome
  dt <- data.frame(id = 1:N) %>%
    mutate(
      # some covariate that determines the outcome
      X = rnorm(N, 0, 2),
      # uniform running variable
      D = floor(runif(N)*100),
      # treatment allocation mechanism
      W = 1*(D >= cut),
      # mean outcome function without treatment
      mu = a + X + (D^2)*b,
      # final outcome function with treatment effect and some error
      Y = mu + W*tau + rnorm(N, 0, sd = 2)
    )
  return(dt)
}
