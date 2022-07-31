library(dplyr)
library(ggplot2)
library(ggridges)

cantometrics = read.csv('processed_data/cantometrics_wunusualness.csv')

cantometrics %>% 
  dplyr::select(society_id, song_id, unusualness_region) %>% 
  arrange(unusualness_region) %>% 
  head()

cantometrics %>% 
  dplyr::select(society_id, song_id, unusualness_region) %>% 
  dplyr::filter(society_id == 24597)


top_bottom_n = 10
cantometrics_topbottom = cantometrics %>%
  group_by(society) %>% 
  summarise(
    n_songs = n(),
    median_unusualness = median(unusualness_region)) %>% 
  dplyr::filter(n_songs >=10) %>% 
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

russia = cantometrics %>% 
  dplyr::filter(Country == "Russia")

ggplot(data = cantometrics_ss,
       aes(x = unusualness_region,
           y = (society),
           col = (society))) + 
  geom_point() + 
  geom_boxplot() + 
  scale_x_continuous(trans = 'reverse') + 
  ylab('') + 
  xlab('Unusualness') + 
  theme_minimal() +
  theme(legend.position = "none") 

ggplot(data = russia,
       aes(x = unusualness_region,
           y = (society),
           col = (society))) + 
  geom_point() + 
  geom_boxplot() + 
  scale_x_continuous(trans = 'reverse') + 
  ylab('') + 
  xlab('Unusualness') + 
  theme_minimal() +
  theme(legend.position = "none") 
  
  
