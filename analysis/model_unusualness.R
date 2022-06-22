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
cantometrics$nearest_distance_std = 
  std_variables(cantometrics$nn_distance_km)
cantometrics$nearest_phyloneighbour_std = 
  std_variables(cantometrics$nearest_phyloneighbour)
cantometrics$n_neighbours_std = 
  std_variables(cantometrics$n_neighbours)
cantometrics$n_glottoneighbours_std = std_variables(cantometrics$n_glottoneighbours)
cantometrics$u_ea_std = std_variables(cantometrics$u_ea)
cantometrics$u_kinship_std = std_variables(cantometrics$u_kinship)
cantometrics$u_economy_std = std_variables(cantometrics$u_economy)
cantometrics$u_housing_std = std_variables(cantometrics$u_housing)
cantometrics$latitude_std = std_variables(cantometrics$Society_latitude)

cantometrics$unusualness_region_std = std_variables(cantometrics$unusualness_region)

cantometrics = cantometrics[complete.cases(cantometrics$u_ea_std),]

results_dir = "results/models/"
dir.create(results_dir, recursive = TRUE, showWarnings = FALSE)

# Null model
fit.1 = brm(
  unusualness_region_std ~ 1 + (1|society_id),
  data = cantometrics,
  chains = chains,
  iter = iter,
  warmup = warmup,
  file = paste0(results_dir, "null_ri.rds")
)
fit.1 <- add_criterion(fit.1, "loo")

# BiVariate models
fit.2.1 = brm(
  unusualness_region_std ~ society_region_diff +
    (1|society_id),
  data = cantometrics,
  chains = chains,
  iter = iter,
  warmup = warmup,
  file = paste0(results_dir, "society_effect.rds")
)
fit.2.1 <- add_criterion(fit.2.1, "loo")

#### Isolation ####
fit.2.2 = brm(
  unusualness_region_std ~ nearest_distance_std + (1|society_id),
  data = cantometrics,
  chains = chains,
  iter = iter,
  warmup = warmup,
  file = paste0(results_dir, "geographic_isolation.rds")
)
fit.2.2 <- add_criterion(fit.2.2, "loo")

fit.2.3 = brm(
  unusualness_region_std ~ nearest_phyloneighbour_std + (1|society_id),
  data = cantometrics,
  chains = chains,
  iter = iter,
  warmup = warmup,
  file = paste0(results_dir, "phylogenetic_isolation.rds")
)
fit.2.3 <- add_criterion(fit.2.3, "loo")

fit.2.4 = brm(
  unusualness_region_std ~ n_neighbours_std + (1|society_id),
  data = cantometrics,
  chains = chains,
  iter = iter,
  warmup = warmup,
  file = paste0(results_dir, "numberof_neighbours.rds")
)
fit.2.4 <- add_criterion(fit.2.4, "loo")

fit.2.4.1 = brm(
  unusualness_region_std ~ n_glottoneighbours_std + (1|society_id),
  data = cantometrics,
  chains = chains,
  iter = iter,
  warmup = warmup,
  file = paste0(results_dir, "numberof_glottoneighbours.rds")
)
fit.2.4.1 <- add_criterion(fit.2.4, "loo")

fit.2.5 = brm(
  unusualness_region_std ~ abs(latitude_std) + (1|society_id),
  data = cantometrics,
  chains = chains,
  iter = iter,
  warmup = warmup,
  file = paste0(results_dir, "latitude.rds")
)
fit.2.5 <- add_criterion(fit.2.5, "loo")

#### Social unusualness model ####
fit.u = brm(
  unusualness_region_std ~ u_ea_std + (1|society_id),
  data = cantometrics,
  chains = chains,
  iter = iter,
  warmup = warmup,
  file = paste0(results_dir, "unusual_EA.rds")
)
fit.u <- add_criterion(fit.u, "loo")

fit.u.k = brm(
  unusualness_region_std ~ u_kinship_std + (1|society_id),
  data = cantometrics,
  chains = chains,
  iter = iter,
  warmup = warmup, 
  file = paste0(results_dir, "unusual_kinship.rds")
)
fit.u.k <- add_criterion(fit.u.k, "loo")

fit.u.e = brm(
  unusualness_region_std ~ u_economy_std + (1|society_id),
  data = cantometrics,
  chains = chains,
  iter = iter,
  warmup = warmup, 
  file = paste0(results_dir, "unusual_economy.rds")
)
fit.u.e <- add_criterion(fit.u.e, "loo")

fit.u.h = brm(
  unusualness_region_std ~ u_housing_std + (1|society_id),
  data = cantometrics,
  chains = chains,
  iter = iter,
  warmup = warmup, 
  file = paste0(results_dir, "unusual_housing.rds")
)
fit.u.h <- add_criterion(fit.u.h, "loo")

#### Summarise bi-variate models
bi_variate = list(Null = fit.1,
                  Regional_Unusualness = fit.2.1,
                  Geographic_Isolation = fit.2.2,
                  Phylogenetic_Isolation = fit.2.3,
                  Number_of_Neighbouts = fit.2.4,
                  Abs_Latitude = fit.2.5,
                  Societal_Unusualness = fit.u,
                  Kinship_Unusualness = fit.u.k,
                  Economic_Unusualness = fit.u.e,
                  Housing_Unusualness = fit.u.h)

models_waic = lapply(bi_variate, function(x) {
  xx = data.frame(x$criteria$waic$estimates)
  xx$type = rownames(xx)
  xx
})
names(models_waic) = names(bi_variate)
outs = map_df(models_waic, ~as.data.frame(.x), .id="id")
outs = subset(outs, type == "elpd_waic")

loo_compare(fit.1, fit.2.1, fit.2.2, fit.2.3, fit.2.4, fit.2.5,
            fit.u, fit.u.e, fit.u.h, fit.u.k)

ggplot(outs, aes(x = Estimate, y = id)) + 
  geom_point() + 
  geom_errorbar(aes(xmin=Estimate-SE, xmax=Estimate+SE), width=.2,
                position=position_dodge(0.05))

## Distance models
fit.d.1 = brm(
  unusualness_region ~ 
    nearest_phyloneighbour_std + 
    nearest_distance_std + 
    (1|society_id),
  data = cantometrics,
  chains = chains,
  iter = iter,
  warmup = warmup, 
  file = paste0(results_dir, "nearest_dist.rds")
)
summary(fit.d.1)

fit.d.2 = brm(
  unusualness_region ~ 
    nearest_phyloneighbour_std + 
    nearest_distance_std + 
    society_region_diff + 
    (1|society_id),
  data = cantometrics,
  chains = chains,
  iter = iter,
  warmup = warmup
)
summary(fit.d.2)

## Combined
fit.5 = brm(
  unusualness_languagefamily ~ 
    nearest_phyloneighbour_std + 
    nearest_distance_std + 
    society_region_diff + 
    n_songs + 
    (nearest_phyloneighbour_std + 
       nearest_distance_std + 
       society_region_diff|FamilyLevGlottocode),
  data = cantometrics,
  chains = chains,
  iter = iter,
  warmup = warmup
)

posterior <- as.matrix(fit.5)

variables = c("b_nearest_phyloneighbour_std", 
              "b_nearest_distance_std",
              "b_society_region_diff",
              "b_n_songs")

fixef(fit.5)
pp_check(fit.5)
mcmc_areas(posterior, prob = 0.8, pars = variables)
summary(fit.5)



