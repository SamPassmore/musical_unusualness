### Total variance vs observed variance
library(stringr)
library(dplyr)
library(scales)
library(corrr)
# make summarise quiet
options(dplyr.summarise.inform = FALSE)

## How many possible codings are there in Cantometrics? 
cantometric_codes = read.csv('raw/gjb/etc/codes.csv')
vars = unique(cantometric_codes$var_id)

possible_codings = prod(table(cantometric_codes$var_id))

cat("There are ", signif(possible_codings, 2), " possible codings in Cantometrics.")

## Observed codings
cantometrics = read.csv("processed_data/cantometrics_wunusualness.csv")

n_songs = nrow(cantometrics)

line_idx = str_detect(colnames(cantometrics), pattern = "line_")
cantometric_lines = cantometrics[,line_idx]

n_codings = 
  cantometric_lines %>% 
  group_by_all(.) %>% 
  summarise(COUNT = n()) %>% 
  nrow()

cat("There are", n_codings, "unique codings from", 
    n_songs, "songs")

cat("The original data set is ", round(n_codings / n_songs, 3) * 100, "% unique")

cat("There are ", n_songs - n_codings, "non-unique codings")

## Randomised diversity
fake_song = function(codes){
  tapply(codes$code, 
         codes$var_id,
         function(x)
           sample(x, 1))
}

iter = 100
fake_proportions = c()
for(i in 1:iter){
  # Create n fake songs
  fake_songs = t(
    sapply(1:n_songs, 
           function(x) fake_song(cantometric_codes))
  )
  fake_songs = data.frame(fake_songs)
  
  # Determine the number of unique recordings
  n_fakecodings = 
    fake_songs %>% 
    group_by_all(.) %>% 
    summarise(COUNT = n()) %>% 
    nrow()
  
  fake_proportions[i] = n_fakecodings / n_songs
}

cat("FAKE SONGS: There are", n_fakecodings, "unique codings from", 
    nrow(cantometrics), " fake songs")


#### Variable Structure
## All variables Network
min_cor = 0.3
 
cantometric_lines %>% correlate() %>% 
  network_plot(min_cor = min_cor, repel = TRUE, curved = FALSE)

## Indo-European societies Network
cantometrics %>% 
  filter(FamilyLevGlottocode == "indo1319") %>% 
  dplyr::select(paste0("line_", 1:37)) %>% 
  correlate() %>% 
  network_plot(min_cor = min_cor, repel = TRUE, curved = FALSE)

cantometrics %>% 
  filter(FamilyLevGlottocode == "aust1307") %>% 
  dplyr::select(paste0("line_", 1:37)) %>% 
  correlate() %>% 
  network_plot(min_cor = min_cor, repel = TRUE, curved = FALSE)


fake_songs %>% correlate() %>% 
  network_plot(min_cor = min_cor)

