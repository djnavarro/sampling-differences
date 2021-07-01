
## 1 July 2021

- I've now added a new analysis of the empirical data that replaces the original one
  (with the "tadpole plots" etc). The findings aren't substantially different, but
  it's a little simpler than the original

## 1 July 2021

- After an initial exploration of the data, I'm not convinced that the framework
  used by Navarro et al (2012) is implemented correctly here. My original thinking was 
  that we could imagine theta playing a similar role in the GP model as it has
  done in other papers: when theta = 1 people apply a "strong sampling" model,
  and when theta = 0 people apply a "weak sampling" model. 
  
  The problem here is 
  that there isn't a very good analog of the "strong vs weak" distinction in 
  the sense described by Tenenbaum and Griffiths' (2001) Bayesian generalisation
  model. Under one interpretation, "strong" sampling could just mean "sampling
  from property-positive stimuli" (i.e., what we would call "property" sampling),
  and "weak" sampling could just mean "sampling independently of the extension
  of the property", per Shepard's (1987) offhand remark about nature selecting the 
  item and the consequential set independently. Under this interpretation, an
  implementation of the Navarro et al (2012) idea should use *two* theta parameters, 
  one for the property condition and another for the category condition, and 
  we would hypothesise that theta_property > theta_category. This version of 
  the model might be worthwhile because it would let us test the hypothesis 
  against the data, but it is *not* what I implemented on 29 June. Rather...
  
  Another interpretation of the Tenenbaum and Griffiths model would assert that
  "strong" sampling means "sampling from a psychologically-relevant set" and
  "weak" sampling means "sampling randomly from the entire set of stimuli". In
  the T&G (2001) paper and the Navarro et al (2012) paper, this definition 
  yields the same likelihood functions as the previous definition, so it doesn't
  matter. However, in the property induction paradigm they give *different*
  answers because this second definition would assert that property sampling 
  and category sampling are *both* examples of a strong sampling regime, since
  the property and the category are both psychologically meaningful things in 
  an inductive reasoning task. In other papers (e.g., the Hayes et al 2019 
  diversity effect paper, the Ransom et al 2016 sampling paper) we have implicitly
  adopted (I think????) something closer to this second definition. That makes
  sense in terms of the goals of those papers (to highlight that people don't
  treat evidence as randomly sampled unless you make it look suuuper random) 
  but it has weird consequences here. In terms of the inferential *behaviour*
  observed when we adopt this definition, category sampling and weak sampling
  end up yielding the same likelihood function, so theta ends up having no
  effect in the category sampling condition. This is the version I originally
  implemented, and I'm not sure it's statistically coherent...
  
  The reason I make that last remark is that our methodological goal with the
  theta parameter is (partly) to capture the "effect" of the sampling frame 
  manipulation, in a manner that is vaguely analogous to how such an effect 
  would be defined in a linear model (except, unlike the linear model the 
  effect would be defined in a way that isn't theoretically garbage). Under 
  interpretation #2, theta behaves like a treatment contrast in which category
  sampling is the default level and property sampling is the treatment level. 
  I don't like this. Empirically, we don't have a good reason to think of 
  category sampling as a default. Literally the only reason to think this is 
  the model, and even this is based on a disputable interpretation of 
  Tenenbaum & Griffiths / Navarro et al... and being the "Navarro" of the latter
  paper I can certainly attest to the fact that I hadn't thought this one 
  through back in 2012. I rather doubt that Josh & Tom had a clear answer in 
  2001 either... though looking at some of the other papers Josh has written
  on property induction tasks almost suggests that he shares Brett's view that
  in these tasks people treat property sampling as a default.
  
  Anyway, the key point here is that interpretation #1 yields something closer
  to the way in which a "cell means" model is described in the ANOVA context and
  could easily be rewritten as a sum to zero contrast. That feels a little more
  statistically palatable to me in this context? 
  
  At the moment what I have done is remove theta from the model implementation
  entirely. It now assumes people use property sampling in the property
  sampling condition and category sampling in the category sampling condition,
  with no continuum between the two permitted. In essence, it means that the 
  model has now reverted to the original Hayes et al (2019) GP model, with no
  additional frills except for the fact that I'm using a grid approximation to 
  the posterior over f rather than an MCMC approximation. I think this is 
  sensible because right now that's the only version of the model I can justify.
  
  Later, I may consider implementing interpretation #1 and try to measure 
  individual subject sensitivity to sampling, but right now I'm not certain if 
  this is either wise or feasible.


## 30 June 2021

- Started tidying my original analysis. One thing that pops out at me in 
  retrospect is that the list of "patterns" I used the first time is too
  long. Most of the "effects" (god I hate that word) in the data set aren't
  particularly interesting. Yes, we replicate the effect that generalisation
  decays with dissimilarity, but that's trivial and not relevant to the 
  specific topic of investigation. Current version reduces this list to those
  that are (according to the model) specifically relevant to the sampling frame
  manipulation

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
- Additional note (added 1 July 2021): the heuristic model that I started this
  repo with does technically constitute an abandoned fork in the analysis
  process. Happily it's more of a "reassuring" one than a "worrying" one. The
  heuristic model I wrote down in September 2020 instantiates the same 
  qualitative biases (sparsity, smoothness, and sensitivity to sampling) as 
  the GP model from Hayes et al (2019), and it produces essentially the same
  pattern of results. In other words, this suggests that we're getting the 
  findings we are getting because of these "core" assumptions, and not because
  of some ancillary property of the GP. 

## 21 September 2020

- Initial model constructed as a simplified version of the Gaussian Process 
  model reported by Hayes et al (2019): [psyarxiv preprint](https://doi.org/10.17605/OSF.IO/2M83V),
  [github repository](https://github.com/djnavarro/samplingframes)
- Note added retrospectively (24 June 2021): if memory serves, at the time I 
  implemented the model in September 2020 I did toy around informally with other 
  parameter values but didn't find anything better than the settings here. I 
  did not implement any other models beside the one described
  [here](https://github.com/djnavarro/sampling-differences/blob/34966459c5c490a22fa7e9b2baddc4fca4397629/models/sampling_frames_model.R)

