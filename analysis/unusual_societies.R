library(dplyr)
library(ggplot2)
library(ggridges)

cantometrics = read.csv('processed_data/cantometrics_wunusualness.csv')

cantometrics_societies = cantometrics %>% 
  group_by(society_id) %>% 
  summarise(
    society_unusualness_ws = median(unusualness_wholesample),
    society_unusualness_lf = median(unusualness_languagefamily),
    song_count = n(),
    Society_latitude = mean(Society_latitude),
    Society_longitude = mean(ifelse(Society_longitude <= -25, 
                               Society_longitude + 360,
                               Society_longitude))
  )

n_top = 10
cantometrics_societies_n = cantometrics_societies %>% 
  dplyr::filter(song_count >9)

top_socs_ws = cantometrics_societies %>% 
  dplyr::filter(song_count >9) %>% 
  slice_max(society_unusualness_ws, n = n_top) %>% 
  pull(society_id)

bottom_socs_ws = cantometrics_societies %>% 
  dplyr::filter(song_count >9) %>% 
  slice_min(society_unusualness_ws, n = n_top) %>% 
  pull(society_id)

top_socs = cantometrics %>% 
  dplyr::filter(society_id %in% top_socs_ws |
                  society_id %in% bottom_socs_ws) %>%
  arrange(unusualness_wholesample)

top_socs$society = factor(top_socs$society, 
                             levels = unique(top_socs$society))
limit = mean(cantometrics$unusualness_wholesample)

ggplot() +
  geom_density_ridges_gradient(data = top_socs, aes(x = unusualness_wholesample, 
                                           y = society, 
                                           group = society,
                                           fill = stat(x))) +
  scale_fill_viridis_c(option = "magma") + 
  scale_x_reverse() + #reverse to make more intuitive
  xlab("Unusualness score") + 
  ylab("Societies") + 
  geom_vline(xintercept = limit, linetype = "dashed") + 
  theme_classic() + 
  theme(legend.position = "none")

top_regions = 
  cantometrics %>% 
  group_by(Region) %>% 
  summarise(n_societies = n_distinct(society_id)) %>% 
  slice_max(order_by = n_societies, n = 4) %>% 
  pull(Region)

regions = cantometrics %>% 
  filter(Region %in% top_regions)

limit <- quantile(cantometrics$unusualness_region, 
                  0.02, 
                  na.rm = T) 

ggplot() +
  geom_boxplot(data = regions, 
                 aes(x = unusualness_wholesample, 
                     y = reorder(factor(society_id),
                                 -unusualness_wholesample, 
                                 na.rm = TRUE),
                     group = factor(society_id))) +
  scale_x_reverse() + #reverse to make more intuitive
  xlab("Unusualness score") + 
  ylab("Societies") + 
  geom_vline(xintercept = limit, linetype = "dashed") + 
  theme_classic() + 
  theme(legend.position = "none") + 
  facet_wrap(~Region, scales = "free_y")

