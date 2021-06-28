library(tidyverse)
library(mvtnorm)
library(magrittr)

# parameter dictionary:
#
# theta = the sampling assumption: 1 = strong, 0 = weak
# tau = baseline in smoothness
# rho = decay in smoothness constraint
# sigma = inherent noise
# mu = prior mean

sampling_frames_model <- function(theta = 1, tau = 3, rho = 1, sigma = .5, mu = .05) {

  # specify the locations of the test items
  x_grid = 1:6

  # specify possible values for the probability
  y_grid <- seq(.1, .9, .2)

  # logit function: phi is a logistic function of f, so
  # f is a logit function of phi
  logit <- function(phi) {
    log(phi/(1-phi))
  }

  # define the psychological distance between values
  distance <- function(xi, xj) {
    abs(xi - xj)
  }

  # define the radial basis kernel function
  rbf_kernel <- function(dist, tau, rho) {
    tau^2 * exp(-rho * dist)
  }

  # construct base variance vector (to be added to the main
  # diagonal of the covariance matrix)
  add_noise <- function(Sigma, sigma) {
    Sigma + diag(sigma^2, nrow(Sigma), ncol(Sigma))
  }

  # distance matrix between items is defined by the
  # outer product...
  distance_matrix <- outer(x_grid, x_grid, distance)

  prior_mean <- function(mu) {
    rep(mu, length(x_grid))
  }

  # compute the priors (this is a placeholder)
  model_prior <- function(hypotheses_tbl) {

    hypotheses_mat <- as.matrix(hypotheses_tbl)

    covariance_matrix <- distance_matrix %>%
      rbf_kernel(tau = tau, rho = rho) %>%
      add_noise(sigma = sigma)

    mean_vector <- prior_mean(mu = mu)

    prior <- logit(hypotheses_mat) %>%
      dmvnorm(mean = logit(mean_vector), sigma = covariance_matrix)

    prior <- prior / sum(prior)

    return(prior)
  }

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
  model_likelihood <- function(n, frame, hypotheses_tbl) {

    if(frame == "property") {

      # property sampling: must be positive property, can be
      # any location on the stimulus space
      denominator <- with(hypotheses_tbl, {
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
    prob1 <- hypotheses_tbl$test1 / denominator
    prob2 <- hypotheses_tbl$test2 / denominator

    # weak sampling equivalent
    weak1 <- hypotheses_tbl$test1 / 6
    weak2 <- hypotheses_tbl$test2 / 6

    # mixture
    prob1 <- theta * prob1 + (1 - theta) * weak1
    prob2 <- theta * prob2 + (1 - theta) * weak2

    # observations are conditionally iid so the probability of the sample
    # is the product  of the individual observations:
    sample_likelihood <- prob1^(n/2) * prob2^(n/2)

    return(sample_likelihood)
  }

  posterior_mean <- function(n, frame, hypotheses_tbl, prior) {

    likelihood <- model_likelihood(n, frame, hypotheses_tbl)
    posterior <- likelihood * prior
    posterior <- posterior / sum(posterior)

    return(tibble(
      test1 = sum(posterior * hypotheses_tbl$test1),
      test2 = sum(posterior * hypotheses_tbl$test2),
      test3 = sum(posterior * hypotheses_tbl$test3),
      test4 = sum(posterior * hypotheses_tbl$test4),
      test5 = sum(posterior * hypotheses_tbl$test5),
      test6 = sum(posterior * hypotheses_tbl$test6),
    ))
  }



  # function to do the work -------------------------------------------------

  infer <- function(hypotheses_tbl) {

    prior <- model_prior(hypotheses_tbl)
    conditions <- as_tibble(expand.grid(
      n = c(2, 8, 20),
      frame = c("category", "property"),
      stringsAsFactors = FALSE
    ))

    generalisations <- conditions %>%
      transpose() %>%
      map_dfr(~posterior_mean(.x$n, .x$frame, hypotheses, prior))

    results <- bind_cols(conditions, generalisations)
    results <- results %>%
      pivot_longer(
        cols = starts_with("test"),
        names_to = "test_item",
        values_to = "response"
      ) %>%
      rename(
        "sampling_frame" = "frame",
        "sample_size" = "n"
      ) %>%
      mutate(
        test_item = as.numeric(str_remove_all(test_item, "test"))
      )

    return(results)
  }


  # specify hypothesis space and run ----------------------------------------

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

  return(infer(hypotheses))

}

model <- sampling_frames_model() %>% mutate(source = "model")
dat <- bind_rows(model, human)
print(plot_curves(dat))

