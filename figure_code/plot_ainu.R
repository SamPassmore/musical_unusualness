library(dplyr)
library(ggplot2)
library(patchwork)

cantometrics = read.csv('processed_data/cantometrics_wunusualness.csv')

ainu = cantometrics %>% 
  dplyr::filter(Language_family == "Ainu") %>%
  mutate(esashi = ifelse(Song == "Esashi Oiwake", "Esashi Oiwake", "Ainu Song"))

whole_sample = ggplot(cantometrics, aes(x = unusualness_wholesample, y = 1)) + 
  geom_boxplot(outlier.shape = NA) + 
  geom_jitter(alpha = 0.1) + 
  geom_jitter(data = ainu,
              aes(x = unusualness_wholesample, 
                  y = 1,
                  size = abs(unusualness_wholesample),
                  col = esashi),
              ) + 
  scale_size_continuous(guide="none") + 
  xlab("Global Unusualness") 

language_family = ggplot(cantometrics, aes(x = unusualness_languagefamily, y = 1)) + 
  geom_boxplot(outlier.shape = NA) + 
  geom_jitter(alpha = 0.1) + 
  geom_jitter(data = ainu,
              aes(x = unusualness_languagefamily, 
                  y = 1,
                  size = abs(unusualness_languagefamily),
                  col = esashi)
  ) + 
  scale_size_continuous(guide="none") + 
  xlab("Unusualness with LF control") 

whole_sample + language_family + 
  plot_annotation("Song Unusualness: smaller values are more unusual", 
                  subtitle = "Emphasis on Ainu") + 
  plot_layout(guides = "collect") & 
  theme(legend.position = 'bottom', 
        legend.title = element_blank())
  
