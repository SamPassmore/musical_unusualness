suppressPackageStartupMessages({
  library(brms)
  library(bayesplot)
  library(dplyr)
  library(ggplot2)
  library(stringr)
  library(tidyr)
  library(ggridges)
  library(patchwork)
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
  theme_minimal(base_size = 15) 

ggsave(plot = p, filename = "figures/figure_4a.png", height = 100, units = "mm", bg="white")

p2 = ggplot(cantometrics, aes(y = unusualness_region, x = society_loo_mean)) +
  geom_point() + 
  ylab("Song Unusualness") + 
  xlab("Societal average unusualness (LOO)") + 
  scale_y_continuous(trans = "reverse") + 
  scale_x_continuous(trans = "reverse") + 
  theme_minimal(base_size = 20) 

ggsave(plot = p2, filename = "figures/figure_4b.png", height = 100, units = "mm", bg="white")

p3 = p + p2

ggsave(plot = p3, filename = "figures/figure_5.png", width = 400, units = "mm", bg="white")
