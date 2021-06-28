library(tidyverse)
library(mvtnorm)
library(magrittr)

# tau = baseline in smoothness
# rho = decay in smoothness constraint
# sigma = inherent noise
# mu = prior mean


sampling_frames_model <- function(
  theta = 1,   # sampling assumption lies between 0 (weak) and 1 (strong)
  tau = 3,     # gaussian process prior: baseline smoothness constraint
  rho = 1,     # gaussian process prior: decay rate for smoothness constraint
  sigma = .5,  # gaussian process prior: inherent noise level in the data
  mu = .05     # prior mean (on the probability scale, not logit scale)
) {


  # useful quantities -------------------------------------------------------

  x_grid = 1:6                # locations of the test items
  y_grid <- seq(.1, .9, .2)   # possible values for the probability

  # construct the hypothesis space: each row specifies the
  # value of the unknown function at each of the 6 test points
  hypotheses <- as_tibble(expand.grid(
    test1 = y_grid,
    test2 = y_grid,
    test3 = y_grid,
    test4 = y_grid,
    test5 = y_grid,
    test6 = y_grid
  ))

  # conditions in the experiment
  conditions <- as_tibble(expand.grid(
    n = c(2, 8, 20),
    frame = c("category", "property"),
    stringsAsFactors = FALSE
  ))



  # convenience functions ---------------------------------------------------

  # logit function: phi is a logistic function of f, so f is a logit of phi
  logit <- function(phi) {
    log(phi/(1-phi))
  }

  # the psychological distance between values is just the difference between
  # coordinates in the stimulus space. this is a little unrealistic, but good
  # enough as an approximation in the current setting
  distance <- function(xi, xj) {
    abs(xi - xj)
  }

  # the kernel function
  kernel <- function(dist) {
    tau^2 * exp(-rho * dist)
  }

  # inherent noise
  noise <- function() {
    diag(sigma^2, length(x_grid), length(x_grid))
  }

  # normalise to 1
  normalise <- function(x) {x / sum(x)}

  # prior mean vector is constant
  prior_mean <- function() {
    logit(rep.int(x = mu, times = length(x_grid)))
  }

  # covariance matrix
  covariance_matrix <- function() {
    distance_matrix <- outer(x_grid, x_grid, distance)
    kernel(distance_matrix) + noise()
  }

  # transform the data frame of hypotheses on the probability
  # scale into a matrix of hypotheses on the logit scale
  logit_scale_hypotheses <- function() {
    logit(as.matrix(hypotheses))
  }



  # gaussian process prior --------------------------------------------------

  model_prior <- function() {
    dmvnorm(
      x = logit_scale_hypotheses(),
      mean = prior_mean(),
      sigma = covariance_matrix()
    ) %>%
      normalise()
  }



  # sampling frame imposes likelihood ---------------------------------------

  # the samples observed by the participant occur only in
  # the locations corresponding to test1 and test2, and are
  # always property positive. depending on condition the
  # participant may have seen 2, 8 or 20 items. for simplicity
  # we assume that they are evenly split between test1 and
  # test 2. conditional on knowing the probabilities of at
  # each test point (and assuming the base rates for entities
  # at each location are uniform) the likelihood is proportional
  # to the function value at the location, where the normalisation
  # depends on the admissability rule specified by the frame

  model_likelihood <- function(cond) {

    # read off condition parameters
    n <- cond$n
    frame <- cond$frame

    if(frame == "property") {

      # property sampling: must be positive property, can be
      # any location on the stimulus space
      denominator <- with(hypotheses, {
        test1 + test2 + test3 + test4 + test5 + test6
      })

    }

    if(frame == "category") {

      # category sampling: can be positive or negative, but can
      # only be locations 1 or 2
      denominator <- 2

    }

    # compute likelihood for the two relevant elements of the sample space
    # (conditional on using the relevant sampling frame)
    prob1 <- hypotheses$test1 / denominator
    prob2 <- hypotheses$test2 / denominator

    # weak sampling equivalent
    weak1 <- hypotheses$test1 / 6
    weak2 <- hypotheses$test2 / 6

    # mixture
    prob1 <- theta * prob1 + (1 - theta) * weak1
    prob2 <- theta * prob2 + (1 - theta) * weak2

    # observations are conditionally iid so the probability of the sample
    # is the product  of the individual observations:
    sample_likelihood <- prob1^(n/2) * prob2^(n/2)

    return(sample_likelihood)
  }



  # compute generalisation gradient -----------------------------------------

  generalisation_gradient <- function(cond, prior) {

    likelihood <- model_likelihood(cond)
    posterior <- normalise(likelihood * prior)

    return(tibble(
      test1 = sum(posterior * hypotheses$test1),
      test2 = sum(posterior * hypotheses$test2),
      test3 = sum(posterior * hypotheses$test3),
      test4 = sum(posterior * hypotheses$test4),
      test5 = sum(posterior * hypotheses$test5),
      test6 = sum(posterior * hypotheses$test6),
    ))
  }


  # function to do the work -------------------------------------------------

  infer <- function() {
    conditions %>%
      transpose() %>%
      map_dfr(generalisation_gradient, prior = model_prior()) %>%
      bind_cols(conditions, .) %>%
      pivot_longer(
        cols = starts_with("test"),
        names_to = "test_item",
        values_to = "response"
      ) %>%
      rename("sampling_frame" = "frame", "sample_size" = "n") %>%
      mutate(test_item = as.numeric(str_remove_all(test_item, "test")))
  }



  # do the work and return to user ------------------------------------------
  return(infer())
}










# test...
library(scico)
source(here::here("analysis","helpers.R"))
exp_data_file <- "exp1.csv"
human <- read_csv(here::here("data", exp_data_file)) %>%
  group_by(sample_size, sampling_frame, test_item) %>%
  summarise(response = mean(response)/10, source = "human") %>%
  ungroup()

model <- sampling_frames_model() %>% mutate(source = "model")
dat <- bind_rows(model, human)
print(plot_curves(dat))

