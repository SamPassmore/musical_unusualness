library(dplyr)

cantometrics = read.csv('processed_data/cantometrics_wunusualness.csv')
cantometrics_societies = read.csv('raw/gjb/cldf/societies.csv')

### The most unusual song
cantometrics %>% 
  arrange(unusualness_region) %>%
  head(2) %>% 
  select(song_id, society_id, unusualness_region, Song, Genre, Song_notes, 
         Performers, Instruments, 
         Vocalist_gender, Lyrics, Recorded_by, Year,
         Publisher, society, Preferred_name, 
         Country, Region, Division)

## All Kurks songs
cantometrics %>% 
  filter(society_id == 24597) %>% 
  select(song_id, society_id, unusualness_region, Song, Genre, Song_notes, 
         Performers, Instruments, 
         Vocalist_gender, Lyrics, Recorded_by, Year,
         Publisher, society, Preferred_name, 
         Country, Region, Division) %>% 
  View()

### Most unusual societies
top_socs = cantometrics %>% 
  group_by(society_id) %>% 
  summarise(unusualness_societymean = mean(unusualness_region),
            nsongs = n()) %>% 
  filter(nsongs > 5) %>% 
  arrange(unusualness_societymean) %>% 
  head(3) %>% 
  pull(society_id)

cantometrics_societies %>% 
  dplyr::filter(society_id %in% top_socs) %>% 
  dplyr::select(society_id,society, Region, Division, Country) %>% 
  View()

# Malay
cantometrics %>% 
  dplyr::filter(society_id %in% top_socs[1]) %>% 
  select(song_id, society_id, unusualness_region, Song, Genre, Song_notes, 
         Performers, Instruments, 
         Vocalist_gender, Lyrics, Recorded_by, Year,
         Publisher, society, Preferred_name, 
         Country, Region, Division) %>% 
  View()

# Kel Aïr Tuareg
cantometrics %>% 
  dplyr::filter(society_id %in% top_socs[2]) %>% 
  select(song_id, society_id, unusualness_region, Song, Genre, Song_notes, 
         Performers, Instruments, 
         Vocalist_gender, Lyrics, Recorded_by, Year,
         Publisher, society, Preferred_name, 
         Country, Region, Division) %>% 
  View()

# Moroccan Berbers
cantometrics %>% 
  dplyr::filter(society_id %in% top_socs[3]) %>% 
  select(song_id, society_id, unusualness_region, Song, Genre, Song_notes, 
         Performers, Instruments, 
         Vocalist_gender, Lyrics, Recorded_by, Year,
         Publisher, society, Preferred_name, 
         Country, Region, Division) %>% 
  View()

### Location of top 2% of societies
limit <- quantile(cantometrics$unusualness_region, 
                  0.02, 
                  na.rm = T) 
cantometrics %>%
  filter(unusualness_region <= limit) %>% 
  group_by(Region) %>% 
  summarise(n = n()) %>%
  mutate(freq = n / sum(n)) %>% 
  arrange(freq)
