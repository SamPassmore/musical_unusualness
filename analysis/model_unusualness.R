## Build model
suppressPackageStartupMessages({
  library(brms)
  library(dplyr)
  library(bayesplot)
  library(purrr)
  library(ggplot2)
})

cantometrics = read.csv('processed_data/cantometrics_modeldata.csv')

# Parameters
chains = 1
iter = 4000
warmup = 2000

## Standardize all predictors
std_variables = function(x) (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)

# Isolation
cantometrics$nearest_distance_std = std_variables(cantometrics$nn_distance_km)
cantometrics$nearest_phyloneighbour_std = std_variables(cantometrics$nearest_phyloneighbour)

# Neighbours
cantometrics$n_neighbours250_std = std_variables(cantometrics$n_neighbours_250)
cantometrics$n_neighbours500_std = std_variables(cantometrics$n_neighbours_500)
cantometrics$n_neighbours1000_std = std_variables(cantometrics$n_neighbours_1000)

# Social Unusualness
cantometrics$u_ea_std = std_variables(cantometrics$u_ea)
cantometrics$u_kinship_std = std_variables(cantometrics$u_kinship)
cantometrics$u_economy_std = std_variables(cantometrics$u_economy)
cantometrics$u_housing_std = std_variables(cantometrics$u_housing)

# society variables
cantometrics$society_loo_mean_std = std_variables(cantometrics$society_loo_mean)

cantometrics = cantometrics[complete.cases(cantometrics$u_ea_std),]

results_dir = "results/models/"
dir.create(results_dir, recursive = TRUE, showWarnings = FALSE)

# Null model
fit.1 = brm(
  unusualness_region ~ 1 + (1|society_id),
  data = cantometrics,
  chains = chains,
  iter = iter,
  warmup = warmup,
  file = paste0(results_dir, "null_ri.rds")
)
fit.1 <- add_criterion(fit.1, "loo")

# Full model
fit.full = brm(
  unusualness_region ~ 
    n_neighbours500_std + 
    nearest_phyloneighbour_std + 
    u_kinship_std + 
    u_economy_std + 
    u_housing_std + 
    (1|society_id),
  data = cantometrics,
  chains = chains,
  iter = iter,
  warmup = warmup,
  file = paste0(results_dir, "full_ri.rds"))
fit.full <- add_criterion(fit.full, "loo")

#### Isolation ####
fit.2.2 = brm(
  unusualness_region ~ nearest_distance_std + (1|society_id),
  data = cantometrics,
  chains = chains,
  iter = iter,
  warmup = warmup,
  file = paste0(results_dir, "geographic_isolation.rds")
)
fit.2.2 <- add_criterion(fit.2.2, "loo")

fit.2.3 = brm(
  unusualness_region ~ nearest_phyloneighbour_std + (1|society_id),
  data = cantometrics,
  chains = chains,
  iter = iter,
  warmup = warmup,
  file = paste0(results_dir, "phylogenetic_isolation.rds")
)
fit.2.3 <- add_criterion(fit.2.3, "loo")

loo_compare(fit.2.2, fit.2.3) # phylogenetic distance preferred

#### Group contact ####
fit.2.4 = brm(
  unusualness_region ~ n_neighbours250_std + (1|society_id),
  data = cantometrics,
  chains = chains,
  iter = iter,
  warmup = warmup,
  file = paste0(results_dir, "numberof_neighbours250.rds")
)
fit.2.4 <- add_criterion(fit.2.4, "loo")

fit.2.4.1 = brm(
  unusualness_region ~ n_neighbours500_std + (1|society_id),
  data = cantometrics,
  chains = chains,
  iter = iter,
  warmup = warmup,
  file = paste0(results_dir, "numberof_neighbours500.rds")
)
fit.2.4.1 <- add_criterion(fit.2.4.1, "loo")

fit.2.4.2 = brm(
  unusualness_region ~ n_neighbours1000_std + (1|society_id),
  data = cantometrics,
  chains = chains,
  iter = iter,
  warmup = warmup,
  file = paste0(results_dir, "numberof_neighbours1000.rds")
)
fit.2.4 <- add_criterion(fit.2.4.2, "loo")

loo(fit.2.4, fit.2.4.1, fit.2.4.2) # 500km distance preferred

#### Social unusualness model ####
fit.u = brm(
  unusualness_region ~ u_ea_std + (1|society_id),
  data = cantometrics,
  chains = chains,
  iter = iter,
  warmup = warmup,
  file = paste0(results_dir, "unusual_EA.rds")
)
fit.u <- add_criterion(fit.u, "loo")

fit.u.k = brm(
  unusualness_region ~ u_kinship_std + (1|society_id),
  data = cantometrics,
  chains = chains,
  iter = iter,
  warmup = warmup, 
  file = paste0(results_dir, "unusual_kinship.rds")
)
fit.u.k <- add_criterion(fit.u.k, "loo")

fit.u.e = brm(
  unusualness_region ~ u_economy_std + (1|society_id),
  data = cantometrics,
  chains = chains,
  iter = iter,
  warmup = warmup, 
  file = paste0(results_dir, "unusual_economy.rds")
)
fit.u.e <- add_criterion(fit.u.e, "loo")

fit.u.h = brm(
  unusualness_region ~ u_housing_std + (1|society_id),
  data = cantometrics,
  chains = chains,
  iter = iter,
  warmup = warmup, 
  file = paste0(results_dir, "unusual_housing.rds")
)
fit.u.h <- add_criterion(fit.u.h, "loo")

loo(fit.u, fit.u.e, fit.u.h, fit.u.k) # kinship is the best model 

## Society mean - 1
fit.society_loo = brm(
  unusualness_region ~ society_loo_mean_std,
  data = cantometrics,
  chains = chains,
  iter = iter,
  warmup = warmup, 
  file = paste0(results_dir, "society_loo.rds")
)
fit.society_loo <- add_criterion(fit.society_loo, "loo")
