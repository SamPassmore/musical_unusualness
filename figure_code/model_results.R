suppressPackageStartupMessages({
  library(brms)
  library(bayesplot)
  library(dplyr)
  library(ggplot2)
  library(stringr)
  library(tidyr)
  library(ggridges)
})

cantometrics = read.csv('processed_data/cantometrics_modeldata.csv')
fit.full = brm(file = "results/models/full_ri.rds")

posterior = brms::as_draws_df(fit.full)
posterior = posterior[,c("b_n_neighbours500_std", 
                         "b_u_economy_std", 
                         "b_nearest_phyloneighbour_std", 
                         "b_u_kinship_std")]
colnames(posterior) =  c(
  "Nearest Geography Neighbours",
  "Economic Unusualness",
  "Nearest Phylogenetic Neighbour",
  "Kinship Unusualness"
)

p = mcmc_areas_ridges(posterior, prob = 0.89) + 
  theme_minimal(base_size = 24) + 
  theme()

ggsave(plot = p, filename = "figures/model_estimates.png", height = 200, units = "mm")

sjPlot::tab_model(fit.full)

p2 = ggplot(cantometrics, aes(y = unusualness_region, x = society_loo_mean)) +
  geom_point() + 
  ylab("Song Unusualness") + 
  xlab("Societal average unusualness (LOO)") + 
  scale_y_continuous(trans = "reverse") + 
  scale_x_continuous(trans = "reverse") + 
  theme_minimal(base_size = 24) 

ggsave(plot = p2, filename = "figures/society_loo.png", height = 200, units = "mm")


cantometrics %>% 
  group_by(Region) %>% 
  summarise(mean_unusualness = mean(unusualness_region))