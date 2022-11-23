## Unusualness of features
suppressPackageStartupMessages({
  library(stringr)
  library(ggplot2)
  library(dplyr)
  library(paletteer)
  library(purrr)
  library(tidyverse)
})


## Functions
Mode <- function(x) {
  ux <- unique(x)
  tabulate(match(x, ux))
  
  
  
  ux[which.max(tabulate(match(x, ux)))]
}

Mode = function(x){
  t = table(x)
  maxx = which(t == max(t))
  values = as.numeric(names(t)[maxx])
  if(length(maxx > 1)){
    
    med = median(values)
    mode = values[which.min(abs(values - med))]
  } else {
    mode = values[maxx]
  }
  mode
}

# x = c(1,2,3,4)
# Mode(x)
# x = c(1, 1, 2, 4)
# Mode(x)

cantometrics = read.csv('processed_data/cantometrics_wunusualness.csv')
line_idx = str_detect(colnames(cantometrics), "line_")

cantometric_codes = read.csv('raw/gjb/etc/codes.csv')
y_labels = read.csv("https://raw.githubusercontent.com/theglobaljukebox/cantometrics/main/etc/variables.csv")

#### Calculate state probabilities ####
regional_idx = cantometrics$Region == "Southeast Asia"
state_probabilities = apply(
  cantometrics[,line_idx],
  2,
  function(x){
    prop.table(table(x))
  }
)

state_probabilities_df = lapply(state_probabilities, function(x){
  dd = data.frame(x)
}) %>% do.call(rbind, .)
colnames(state_probabilities_df) = c("state", "frequency")
state_probabilities_df$var_id = 
   str_extract(rownames(state_probabilities_df), pattern = ".*?\\.") %>% 
   str_remove("\\.")

state_probabilities_df$state =
  as.numeric(
    as.character(
      state_probabilities_df$state
      )
    )


## Prepare for plotting
df <- state_probabilities_df %>% 
  group_by(state, var_id) %>%
  summarize(frequency = sum(frequency)) %>% 
  group_by(var_id) %>% 
  mutate(n_states = n(),
         thirteen_split = as.numeric(cut(1:n_states, 13, labels = FALSE))) %>% 
  left_join(., y_labels, by = c("var_id" = "id")) %>% 
  mutate(state = factor(state, levels = 1:max(as.numeric(state))),
        var_id = str_replace(var_id, "_", " "),
        var_id = str_to_title(var_id),
        var_id = factor(var_id, levels = paste0("Line ", 1:37))
        ) %>% 
  arrange(var_id) %>% 
  mutate(short_title = factor(short_title, unique(short_title)))

rescale_states = df %>% 
  select(state, var_id, short_title, thirteen_split)

## Get songs of interest 
society_ofinterest = c(20571)
song_details = cantometrics %>% 
  dplyr::filter(society_id %in% society_ofinterest) %>% 
  select(song_id, society_id, starts_with("line_"))

song_long = pivot_longer(song_details, cols = starts_with("line_")) %>% 
  left_join(., y_labels[,c("id", "short_title")], by = c("name" = "id")) %>% 
  mutate(short_title = factor(short_title, levels = levels(df$short_title)),
         value = factor(value)) %>% 
  left_join(., rescale_states, by = c("value" = "state", "short_title"))

song_names = song_details$song_id

song_mode = song_long %>% 
  group_by(short_title) %>% 
  summarise(modal_value = Mode(thirteen_split))

head(song_mode)

p = ggplot() + 
  geom_line(data = song_long, aes(x = short_title, y = thirteen_split, group = song_id), col = "grey", alpha = 0.5) + 
  geom_line(data = song_mode, aes(x = short_title, y = modal_value, group = 1), col = "red", lwd = 1.0) + 
  geom_point(data = df, aes(y = thirteen_split, x = short_title, size = frequency), 
             color = "black", fill = "white") + 
  coord_flip() + 
  scale_x_discrete(limits = rev) + 
  xlab(element_blank()) + 
  ylab(element_blank()) + 
  scale_size_continuous(range = c(2, 13)) + 
  theme_minimal(base_size = 24) + 
  theme(legend.position = "bottom",
        panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
  labs(size = "Proportion of feature occurance") + 
  ggtitle("Profile of Malay songs and modal profile")

ggsave(plot = p, filename = "figures/malay_stateprobabilities.png", height = 400, units = "mm")


