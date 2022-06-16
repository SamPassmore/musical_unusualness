## Unusualness of features
library(stringr)
library(ggplot2)
library(dplyr)
library(paletteer)

cantometrics = read.csv('processed_data/cantometrics_wunusualness.csv')
line_idx = str_detect(colnames(cantometrics), "line_")

cantometric_codes = read.csv('raw/gjb/etc/codes.csv')

#### Calculate state probabilities ####
regional_idx = cantometrics$Region == "Africa"
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

song_details = function(cantometrics, id){
  sd = subset(cantometrics, song_id == id)
  sd = sd[,str_detect(colnames(sd), "line_")]
  sd
}

song_details = subset(cantometrics, song_id %in% c(30076, 30077))
song_title = paste0(song_details$Song, " by ",
                    song_details$society)
song_details = song_details[,
                            str_detect(colnames(song_details), 
                                       "line_")
                            ]

df <- state_probabilities_df %>% 
  group_by(state, var_id) %>%
  summarize(frequency = sum(frequency))

# Get locations for each song in song_details
freq_list = list()
for(i in 1:nrow(song_details)){
  freq <- unlist(Map(function(d, val) {
    (sum(d$frequency[d$state > val]) + 0.5 * d$frequency[d$state == val]) /
      sum(d$frequency)
  }, d = split(df, df$var_id), val = song_details[i,]))
  freq_list[[i]] = freq
}


line_list = list()
for(i in 1:nrow(song_details)){
  line_ids = str_replace(colnames(song_details), "_", " ") %>%
    str_to_title(.)
  line_ids = factor(line_ids,  levels = paste0("Line ", 1:37))
  state = factor(song_details[i,], levels = 13:1)
  
  line_list[[i]] = data.frame(state = state, 
                         frequency = freq_list[[i]], 
                         var_id = line_ids)
}
line_df <- data.frame(state = unlist(song_details), 
                      frequency = freq, 
                      var_id = names(song_details))

## Reorder for ease of reading
df$state = factor(
  df$state,
  levels = 1:max(df$state)
  )

df$var_id = str_replace(df$var_id, "_", " ") %>%
  str_to_title(.)

df$var_id = factor(df$var_id,
                   levels = paste0("Line ", 1:37))

# line_df$var_id = str_replace(line_df$var_id, "_", " ") %>%
#   str_to_title(.)

# line_df$var_id = factor(line_df$var_id, 
#                         levels = paste0("Line ", 1:37))
# line_df$state = factor(line_df$state, levels = 13:1)

names(freq) = str_replace(names(freq), "_", " ") %>%
  str_to_title(.)

p = ggplot(df, aes(fill=state, y=frequency, x=var_id)) + 
  geom_col(position="fill", color = "white") +
  geom_line(data = line_list[[1]], aes(group = 1), col = "blue", lwd = 1.) +
  geom_point(data = line_list[[1]], aes(group = 1), col = "blue") + 
  geom_line(data = line_list[[2]], aes(group = 1), col = "red", lwd = 1.) +
  geom_point(data = line_list[[2]], aes(group = 1), col = "red") + 
  ggtitle("Panpipe Ensemble by Kursk (Blue) vs A'Wach Ritual by Moroccan Berbers (Red)") +
  ylab("Proportion of Categories") +
  xlab(element_blank()) + 
  theme_minimal() + 
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        axis.text.x = element_text(angle = 90)) + 
  guides(fill=guide_legend(nrow = 2,
                           byrow = TRUE,
                           override.aes = list(shape = NA)))

ggsave(plot = p, 
       filename = paste0(
         "figures/state_probabilities_", 
         "two_songs", 
         ".jpg"
         ))
