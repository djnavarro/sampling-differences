library(tidyverse)
library(scico)

sampling_frames_model <- function(theta = 1, smooth_weight = 1, mean_weight = 2) {
  
  # construct the hypothesis space: each row specifies the 
  # value of the unknown function at each of the 6 test points
  grid <- seq(.1, .9, .2)
  hypotheses <- as_tibble(expand.grid(
    test1 = grid,
    test2 = grid,
    test3 = grid,
    test4 = grid,
    test5 = grid,
    test6 = grid
  ))
  
  # compute the priors (this is a placeholder)
  model_prior <- function(hypotheses) {
    
    corr_distance <- with(hypotheses, {
      abs(test2 - test1) + 
        abs(test3 - test2) +
        abs(test4 - test3) +
        abs(test5 - test4) + 
        abs(test6 - test5)
    })
    
    mean_val <- 0
    mean_distance <- with(hypotheses, {
      abs(test1 - mean_val) + 
        abs(test2 - mean_val) + 
        abs(test3 - mean_val) + 
        abs(test4 - mean_val) + 
        abs(test5 - mean_val) + 
        abs(test6 - mean_val) 
    })
    
    distance <- corr_distance * smooth_weight + 
      mean_distance * mean_weight  
    
    prior <- exp(-distance)
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
  model_likelihood <- function(n, frame, hypotheses) {
    
    # theta is inherited from scope!
    
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
  
  posterior_mean <- function(n, frame, hypotheses, prior) {
    
    likelihood <- model_likelihood(n, frame, hypotheses)
    posterior <- likelihood * prior
    posterior <- posterior / sum(posterior)
    
    return(tibble(
      test1 = sum(posterior * hypotheses$test1),
      test2 = sum(posterior * hypotheses$test2),
      test3 = sum(posterior * hypotheses$test3),
      test4 = sum(posterior * hypotheses$test4),
      test5 = sum(posterior * hypotheses$test5),
      test6 = sum(posterior * hypotheses$test6),
    ))
  }
  
  infer <- function(hypotheses) {
    
    prior <- model_prior(hypotheses)
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
  
  return(infer(hypotheses))
  
}