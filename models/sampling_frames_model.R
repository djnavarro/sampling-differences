
sampling_frames_model <- function(
  theta = 1,   # sampling assumption lies between 0 (weak) and 1 (strong)
  tau = 3,     # gaussian process prior: baseline smoothness constraint
  rho = 1,     # gaussian process prior: decay rate for smoothness constraint
  sigma = .5,  # gaussian process prior: inherent noise level in the data
  mu = .05     # prior mean (on the probability scale, not logit scale)
) {

  `%>%` <- magrittr::`%>%` # use the pipe in the local environment

  # useful quantities -------------------------------------------------------

  x_grid = 1:6                # locations of the test items
  y_grid <- seq(.1, .9, .2)   # possible values for the probability

  # construct the hypothesis space: each row specifies the
  # value of the unknown function at each of the 6 test points. note that
  # this is constructed on the *raw* (i.e., probability) scale. not
  # the transformed (i.e., logit) scale. this means that the likelihood
  # functions can use these values transparently, but the prior needs to
  # logit-transform them
  hypotheses <- tibble::as_tibble(expand.grid(
    test1 = y_grid,
    test2 = y_grid,
    test3 = y_grid,
    test4 = y_grid,
    test5 = y_grid,
    test6 = y_grid
  ))

  # conditions in the experiment
  conditions <- tibble::as_tibble(expand.grid(
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
    mvtnorm::dmvnorm(
      x = logit_scale_hypotheses(),
      mean = prior_mean(),
      sigma = covariance_matrix()
    ) %>%
      normalise()
  }



  # the three possible sampling models --------------------------------------

  # sampling models when the items are selected *from* the extension
  # of the category, or the extension of the property... for property
  # sampling the item can be sampled at any location in the stimulus
  # space so long as it is property-positive...
  property_sampling <- function(stimulus) {
    numerator <- eval(
      expr = substitute(stimulus),
      envir = hypotheses
    )
    denominator <- eval(
      expr = quote(test1 + test2 + test3 + test4 + test5 + test6),
      envir = hypotheses
    )
    return(numerator / denominator)
  }

  # category sampling is also a strong sampling model, but where we
  # are restricted to sampling items that belong to some subset of the
  # stimulus space (regardless of whether they are property positive).
  # In this context, that subset is simply the first two test items.
  category_sampling <- function(stimulus) {
    numerator <- eval(
      expr = substitute(stimulus),
      envir = hypotheses
    )
    denominator <- 2
    return(numerator / denominator)
  }

  # weak sampling assumes the items are sampled uniformly at random from
  # all items, and the fact that the observed item turned out to be property
  # positive is coincidental. in this experimental design there is not much
  # of a difference between weak sampling and category sampling
  weak_sampling <- function(stimulus) {
    numerator <- eval(
      expr = substitute(stimulus),
      envir = hypotheses
    )
    denominator <- 6
    return(numerator / denominator)
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

    if(cond$frame == "property") {
      strong_prob1 <- property_sampling(test1)
      strong_prob2 <- property_sampling(test2)
    }
    if(cond$frame == "category") {
      strong_prob1 <- category_sampling(test1)
      strong_prob2 <- category_sampling(test2)
    }
    weak_prob1 <- weak_sampling(test1)
    weak_prob2 <- weak_sampling(test2)

    # the learner is assumed to apply a mixture of strong and weak sampling,
    # per the model in Navarro et al (2012). in practice, because weak and
    # category sampling are indistinguishable in this design, the role of
    # the theta parameter is to shift the generalisation gradients in the
    # property sampling condition only (in the direction of the category/weak
    # sampling gradients)
    prob1 <- theta * strong_prob1 + (1 - theta) * weak_prob1
    prob2 <- theta * strong_prob2 + (1 - theta) * weak_prob2

    # observations are conditionally iid so the probability of the sample
    # is the product of the individual observations:
    sample_likelihood <- prob1^(cond$n/2) * prob2^(cond$n/2)

    return(sample_likelihood)
  }



  # compute generalisation gradient -----------------------------------------

  expected_value <- function(probability, value) {
    sum(probability * value)
  }

  # to compute the generalisation gradient we first compute the posterior
  # distribtution over hypotheses, given the observed data and the sampling
  # condition. then, for each test item, the posterior expected probability
  # that a new item possesses the property is just the expected value of
  # the unknown function at that location in the stimulus space:

  generalisation_gradient <- function(cond, prior) {

    likelihood <- model_likelihood(cond)
    posterior <- normalise(likelihood * prior)
    generalisation <- tibble::tibble(
      test1 = expected_value(posterior, hypotheses$test1),
      test2 = expected_value(posterior, hypotheses$test2),
      test3 = expected_value(posterior, hypotheses$test3),
      test4 = expected_value(posterior, hypotheses$test4),
      test5 = expected_value(posterior, hypotheses$test5),
      test6 = expected_value(posterior, hypotheses$test6)
    )
    return(generalisation)
  }



  # do the work and return to user ------------------------------------------

  # this is the important part of the computation
  result <- conditions %>%
    purrr::transpose() %>%
    purrr::map_dfr(generalisation_gradient, prior = model_prior()) %>%
    dplyr::bind_cols(conditions, .)

  # clean it up and make it nice and pretty
  output <- result %>%
    tidyr::pivot_longer(
      cols = tidyselect::starts_with("test"),
      names_to = "test_item",
      values_to = "response"
    ) %>%
    dplyr::rename(
      sampling_frame = frame,
      sample_size = n
    ) %>%
    dplyr::mutate(
      test_item = as.numeric(stringr::str_remove_all(test_item, "test")),
      source = "model"
    )

  return(output)
}
