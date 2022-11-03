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
min_songs = 5

cantometrics = read.csv('processed_data/cantometrics_wunusualness.csv')

sum(lomax_ids %in% cantometrics$society_id)

cantometrics_societies = cantometrics %>%
  group_by(society) %>% 
  summarise(
    society_id = first(society_id),
    n_songs = n(),
    median_unusualness = mean(unusualness_region))

## add a percentile
cantometrics_societies = cantometrics_societies[order(cantometrics_societies$median_unusualness),]
n = nrow(cantometrics_societies)
cantometrics_societies$percentile = (1:n - 1)/(n - 1)

cantometrics_societies %>% 
  filter(society_id %in% lomax_ids)

lomax_isolates = cantometrics %>% 
  filter(society_id %in% lomax_ids) %>% 
  select(song_id, society_id, 
         unusualness_wholesample,
         unusualness_region,
         unusualness_languagefamily,
         percentile) %>% 
  group_by(society_id) %>% 
  summarise(unusualness_wholesample = mean(unusualness_wholesample),
          unusualness_region = mean(unusualness_region),
          unusualness_languagefamily = mean(unusualness_languagefamily),
          percentile = mean(percentile)
          )


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
