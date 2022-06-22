## Random Cantometrics
options(dplyr.summarise.inform = FALSE)
library(dplyr)

cantometrics = read.csv('raw/gjb/cldf/songs.csv')
cantometric_codes = read.csv('raw/gjb/etc/codes.csv')

## Randomised diversity
fake_song = function(codes){
  tapply(codes$code, 
         codes$var_id,
         function(x)
           sample(x, 1))
}

n_songs = nrow(cantometrics)
iter = 100
fake_proportions = c()
fake_correlations = list()
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
  
  # Fake correlations
  fake_corrs = cor(fake_songs)
  fake_correlations[[i]] = summary(fake_corrs[lower.tri(fake_corrs)])
}

cat("FAKE SONGS: There are", n_fakecodings, "unique codings from", 
    nrow(cantometrics), " fake songs")

correlations = do.call(rbind, fake_correlations)
correlation_summary = colMeans(correlations)
cat("FAKE SONGS: The correlations between variables are on average:",
    round(correlation_summary[4], 3))
    


    