
## 29 June 2021

- Model updated to more closely match the GP model from Hayes et al (2019).
  The main difference in implementation is that I don't use JAGS to sample from
  the posterior over functions/hypotheses. Instead a fixed and "small" (~15000)
  set of functions is used to provide a crude grid approximation. This works 
  only because we aren't interested in the posterior distribution itself, only
  the posterior mean at each of the test stimulus locations.
- Note: the default parameters currently specified for `sampling_frames_model()`
  are not precisely identical to those in Hayes et al. I did a small amount of
  hand tuning against the data from Experiment 1, but nothing particularly 
  thorough.

## 21 September 2020

- Initial model constructed as a simplified version of the Gaussian Process 
  model reported by Hayes et al (2019): [psyarxiv preprint](https://doi.org/10.17605/OSF.IO/2M83V),
  [github repository](https://github.com/djnavarro/samplingframes)
- Note added retrospectively (24 June 2021): if memory serves, at the time I 
  implemented the model in September 2020 I did toy around informally with other 
  parameter values but didn't find anything better than the settings here. I 
  did not implement any other models beside the one described
  [here](https://github.com/djnavarro/sampling-differences/blob/34966459c5c490a22fa7e9b2baddc4fca4397629/models/sampling_frames_model.R)

