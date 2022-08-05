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
    median_unusualness = mean(unusualness_region)) %>% 
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

       