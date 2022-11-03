## Unusualness of features
suppressPackageStartupMessages({
  library(stringr)
  library(ggplot2)
  library(dplyr)
  library(paletteer)
  library(purrr)
})

cantometrics = read.csv('processed_data/cantometrics_wunusualness.csv')
line_idx = str_detect(colnames(cantometrics), "line_")

cantometric_codes = read.csv('raw/gjb/etc/codes.csv')

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

song_details = function(cantometrics, id){
  sd = subset(cantometrics, song_id == id)
  sd = sd[,str_detect(colnames(sd), "line_")]
  sd
}

# songs = c(30076, 30077)
# song_details = subset(cantometrics, song_id %in% songs)

society_ofinterest = c(20571)
song_details = cantometrics %>% 
  dplyr::filter(society_id %in% society_ofinterest)

song_names = song_details$song_id

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

names(line_list) = song_names
line_list = map_df(line_list, ~as.data.frame(.x), .id="id")

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



p = ggplot(df, aes(y=frequency, x=var_id)) + 
  geom_col(position="fill", color = "black", fill = "white") +
  geom_line(data = line_list, aes(group = id, col = id), lwd = 1.) +
  # geom_point(data = line_list, aes(group = id, col = id)) +
  # song of interest
  geom_line(data = line_list[line_list$id == 2453,], aes(group = id), lwd = 1., col = "red") +
  ylab("Proportion of Categories") +
  xlab(element_blank()) + 
  theme_minimal() + 
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        axis.text.x = element_text(angle = 90)) + 
  scale_color_manual(values = c("2453" = "grey", 
                                "2447" = "grey",
                                "2448" = "grey",
                                "2449" = "grey",
                                "2450" = "grey",
                                "2451" = "grey",
                                "2452" = "grey",
                                "2454" = "grey",
                                "2455" = "grey",
                                "2456" = "grey",
                                "419" = "grey")) + 
  guides(fill=guide_legend(nrow = 2,
                           byrow = TRUE,
                           override.aes = list(shape = NA)),
         col = "none")

p
ggsave(plot = p, 
       filename = paste0(
         "figures/state_probabilities_", 
         "two_songs", 
         ".jpg"
         ), 
       bg="white")

df = df %>% 
  group_by(var_id) %>% 
  mutate(n_states = n(),
         thirteen_split = cut(1:n_states, 13, labels = FALSE))

states = df %>% 
  group_by(var_id) %>% 
  distinct(frequency, thirteen_split)

line_list = df %>% 
  group_by(var_id) %>% 
  mutate(n_states = n(),
         thirteen_split = cut(1:n_states, 13, labels = FALSE))

ggplot(df, aes(y = thirteen_split, x = var_id, size = frequency)) + 
  geom_point() +
  geom_line(data = line_list[line_list$id == 2453,], 
            aes(y = as.numeric(state), group = id), lwd = 1.) +
  theme_minimal() + 
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        axis.text.x = element_text(angle = 90)) 

p = ggplot(df, aes(y=1:13, x=var_id, size = frequency)) + 
  geom_point(position="fill", color = "black", fill = "white") +
  geom_line(data = line_list, aes(group = id, col = id), lwd = 1.) +
  # geom_point(data = line_list, aes(group = id, col = id)) +
  # song of interest
  geom_line(data = line_list[line_list$id == 2453,], aes(group = id), lwd = 1., col = "red") +
  ylab("Proportion of Categories") +
  xlab(element_blank()) + 
  theme_minimal() + 
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        axis.text.x = element_text(angle = 90)) + 
  scale_color_manual(values = c("2453" = "grey", 
                                "2447" = "grey",
                                "2448" = "grey",
                                "2449" = "grey",
                                "2450" = "grey",
                                "2451" = "grey",
                                "2452" = "grey",
                                "2454" = "grey",
                                "2455" = "grey",
                                "2456" = "grey",
                                "419" = "grey")) + 
  guides(fill=guide_legend(nrow = 2,
                           byrow = TRUE,
                           override.aes = list(shape = NA)),
         col = "none")
