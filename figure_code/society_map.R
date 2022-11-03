library(dplyr)
library(ggplot2)
library(patchwork)

cantometrics = read.csv('processed_data/cantometrics_wunusualness.csv')

mapping_variable = "unusualness_region"

# Get average society score and song counts
cantometrics_societies = cantometrics %>% 
  add_count(society_id, name = "song_count") %>% 
  group_by(society_id) %>% 
  summarise(
    unusualness_wholesample = median(unusualness_wholesample),
    unusualness_languagefamily = median(unusualness_languagefamily),
    unusualness_region = median(unusualness_region),
    song_count = mean(song_count),
    Society_latitude = mean(Society_latitude),
    Society_longitude = ifelse(Society_longitude <= -25, 
                                Society_longitude + 360,
                                Society_longitude)
  ) %>% 
  distinct(.keep_all = TRUE) %>%
  dplyr::filter(song_count >= 5)

#### Plot map
world <- 
  ggplot2::map_data('world2', wrap=c(-25,335), 
                    # rewrapping the worldmap, Pacific center. 
                    ylim=c(-55,90)) # cutting out antarctica (not obligatory) and the northermost part where there are no language points in glottolog

basemap <- ggplot() +
  geom_polygon(data=world, aes(x=long, #plotting the landmasses
                               y=lat,group=group),
               colour="gray90",
               fill="gray90", size = 0.5) +
  theme(#all of theme options are set such that it makes the most minimal plot, no legend, not grid lines etc
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    axis.title.x=element_blank(),
    axis.title.y=element_blank(),
    axis.line = element_blank(),
    panel.border = element_blank(),
    panel.background = element_rect(fill = "white"),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    axis.ticks.length = unit(0, "mm"))   +
  coord_map(projection = "vandergrinten") + #a non-rectangular world map projection that is a decen compromise between area and distances accuracy
  coord_map(projection = "vandergrinten", ylim=c(-56,67)) +
  expand_limits(x = cantometrics_societies$Society_longitude, 
                y = cantometrics_societies$Society_latitude)

map_r = basemap + 
  geom_point(data = cantometrics_societies,
             aes(x = Society_longitude,
                 y = Society_latitude,
                 size = song_count,
                 col = .data[[mapping_variable]]),
             alpha = 0.6) +
  scale_color_viridis_c(option = "magma") + 
  theme(legend.position = "none")


#### Histogram
limit <- quantile(cantometrics[[mapping_variable]], 
                  0.02, 
                  na.rm = T)  # top 2 %

## Add lines for Lomax Unusual socieitets
lomax_ids = c(27413, 62311, 21869, 12816)
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

histogram_plot = ggplot(cantometrics, 
      aes(unusualness_region, fill=..x..)) +
  geom_histogram(bins = 40) +
  annotate("segment",
           col="black",
           alpha = 0.6,
           x = limit,
           xend = limit,
           y = 0, yend = 400, size = 0.5, linetype = "dashed") +
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

mh <- map_r + 
  inset_element(histogram_plot, 
                right = 0.5, 
                bottom = 0.0, 
                left = 0.2, 
                top = 0.3)

ggsave("figures/worldmap_region.png", 
       mh, 
       height = 5, 
       width = 8)

