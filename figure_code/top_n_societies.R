suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(ggridges)
})

cantometrics = read.csv('processed_data/cantometrics_wunusualness.csv')

top_bottom_n = 10
min_songs = 5
cantometrics_topbottom = cantometrics %>%
  group_by(society) %>% 
  summarise(
    n_songs = n(),
    median_unusualness = median(unusualness_region)) %>% 
  dplyr::filter(n_songs >= min_songs) %>% 
  arrange(median_unusualness) %>%
  slice(1:top_bottom_n, (n()-top_bottom_n):n(), with_ties = FALSE) %>% 
  pull(society)

cantometrics_ss = cantometrics %>% 
  dplyr::filter(society %in% cantometrics_topbottom)

cantometrics_ss$society = factor(cantometrics_ss$society,
                                 levels = cantometrics_topbottom)

cantometrics_ss$topbottom = ifelse(cantometrics_ss$society %in% cantometrics_topbottom[1:10], 
                                "Most Unusual",
                                "Least Unusual")

# remove societies that don't match the metadata
cantometrics_ss = cantometrics_ss[!is.na(cantometrics_ss$society),]

p = ggplot(data = cantometrics_ss,
       aes(x = unusualness_region,
           y = society)) + 
  geom_boxplot() + 
  geom_point(alpha = 0.5) + 
  scale_x_continuous(trans = 'reverse') + 
  ylab('') + 
  xlab('Unusualness') + 
  theme_minimal(base_size = 24) +
  theme(legend.position = "none") +
  facet_wrap(~topbottom, drop = TRUE, scales = 'free_y',
             nrow = 2) + 
  scale_y_discrete(labels=c("Malay"=expression(bold("Malay")), 
                            "Kel Aïr Tuareg" = expression(bold("Kel Aïr Tuareg")),
                            "Moroccan Berbers" = expression(bold("Moroccan Berbers")),
                            parse=TRUE))

ggsave(plot = p, file = "figures/top_n_unusual.png", height = 290, units = "mm")
       