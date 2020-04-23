library(tidyverse)
library(here)
library(brms)

CHAINS <- 4
ITER <- 4000
WARMUP <- 2000
BAYES_SEED <- 1234
options(mc.cores = parallel::detectCores())

BLS <- read_rds(here("data", "derived_data", "BLS_small.rds"))

set.seed(1234)
BLS_small <- BLS %>%
  sample_frac(0.25)

# This takes so long! 370ish seconds per 1000 transitions - takes 2ish hours
# to run the whole thing!! (6500ish seconds per chain!)
model_zinb <- brm(
  brmsformula(landscape_fitness_linked ~ create_network + disperse + compete +
                catastrophe + selectfor_d + select + numlink + cdis1 +
                var_fitness + max_intial_proportion_links + maxsp),
  data = BLS_small, family = zero_inflated_beta(),
  chains = CHAINS, iter = ITER, warmup = WARMUP, seed = BAYES_SEED
)

saveRDS(model_zinb, here("data", "derived_data", "model_zinb_test.rds"))
