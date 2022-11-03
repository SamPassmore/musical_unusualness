suppressPackageStartupMessages({
  library(brms)
  library(bayesplot)
  library(dplyr)
  library(ggplot2)
  library(stringr)
  library(tidyr)
  library(ggridges)
})


fit.full = brm(file = "results/models/full_ri.rds")

posterior = brms::as_draws_df(fit.full)
posterior = posterior[,c("b_n_neighbours500_std", 
                         "b_u_economy_std", 
                         "b_u_housing_std", 
                         "b_nearest_phyloneighbour_std", 
                         "b_u_kinship_std")]
colnames(posterior) =  c(
  "N Neighbours",
  "Economic Unusualness",
  "Housing Unusualness",
  "Nearest Phylo Neighbour",
  "Kinship Unusualness"
)

mcmc_areas_ridges(posterior, prob = 0.89)

sjPlot::tab_model(fit.full)
