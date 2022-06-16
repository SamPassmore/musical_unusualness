## Lomax Unusual societies
library(dplyr)
library(ggplot2)

# Alan Lomax labels these four societies as Unique isolates
# Within Factors of Musical Style. 
# Totonac (27413)
# Miao (62311)
# Munda (21869)
# Pentacost (12816) - Not 100% sure that this is the right society

lomax_ids = c(27413, 62311, 21869, 12816)

cantometrics = read.csv('processed_data/cantometrics_wunusualness.csv')

lomax_isolates = cantometrics %>% 
  filter(society_id %in% lomax_ids) %>% 
  select(song_id, society_id, 
         unusualness_wholesample,
         unusualness_region,
         unusualness_languagefamily) %>% 
  group_by(society_id) %>% 
  summarise(unusualness_wholesample = mean(unusualness_wholesample),
          unusualness_region = mean(unusualness_region),
          unusualness_languagefamily = mean(unusualness_languagefamily
                                            ))


ggplot(lomax_isolates, 
       aes(unusualness_region, fill=..x..)) +
  geom_histogram(bins = 40) +
  geom_histogram(data = cantometrics, 
                 aes(unusualness_region, fill=..x..), alpha = 0.3) + 
  scale_fill_viridis_c('Score', option="A", direction=-1) +
  xlab("Unusualness Score") + ylab("Number of Songs") +
  guides(fill="none") +
  scale_x_reverse() + #reverse to make more intuitive
  theme_classic() +
  theme(axis.title = element_text(size = 8),
        panel.background = 
          element_rect(fill = "transparent", colour = NA),  
        plot.background = 
          element_rect(fill = "transparent", colour = NA),
        legend.position = "none")
