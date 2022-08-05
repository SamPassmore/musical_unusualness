### Total variance vs observed variance
suppressPackageStartupMessages({
  library(stringr)
  library(dplyr)
  library(scales)
  library(corrr)
})

# make summarise quiet
options(dplyr.summarise.inform = FALSE)

## How many possible codings are there in Cantometrics? 
cantometric_codes = read.csv('raw/gjb/etc/codes.csv', fileEncoding="UTF-8-BOM")
vars = unique(cantometric_codes$var_id)

possible_codings = prod(table(cantometric_codes$var_id))

cat("There are ", format(possible_codings, scientific = FALSE, big.mark=","), " possible codings in Cantometrics.\n")

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
    n_songs, "songs\n")

cat("The original data set is ", round(n_codings / n_songs, 3) * 100, "% unique.\n")

cat("There are ", n_songs - n_codings, "non-unique codings.\n")

## Randomised diversity
fake_song = function(codes){
  tapply(codes$code, 
         codes$var_id,
         function(x)
           sample(x, 1))
  ## Add dependencies 
}

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
  
  fake_correlations[[i]] = correlate(fake_songs, 
                                     method = "pearson",
                                     use = "pairwise.complete.obs", 
                                     quiet = TRUE)
}

cat("FAKE SONGS: There are", n_fakecodings, "unique codings from", 
    nrow(cantometrics), " fake songs.\n")
cat("Across all iterations, the simulated cantometrics set contains",
    paste0(formatC(100 * mean(fake_proportions), format = "f", digits = 1), "%"),
    "unique fake songs")

# summarise correlations
summary_fakecorrelations = sapply(fake_correlations, function(f){
  f = data.matrix(f)
  f = f[,2:ncol(f)]
  mean(f[lower.tri(f)], na.rm = TRUE)
})

average_fakecorrelation = mean(summary_fakecorrelations)

cat("Across ", iter, "iterations, simulated Cantometrics variables had an average pearson correlation of",
    format(average_fakecorrelation, scientific = FALSE, big.mark=",", digits = 2))
