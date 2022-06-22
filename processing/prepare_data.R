suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(scales)
  library(stringr)
  library(assertthat)
  library(readxl)
})

source('functions/helper.R')

set.seed(2897)

cantometrics_data = read.csv('raw/gjb/cldf/data.csv', na.strings = "") 
cantometrics_societies = read.csv('raw/gjb/cldf/societies.csv')

cantometric_songs = read.csv('raw/gjb/cldf/songs.csv')

# some songs are double coded for particular features. 
# However, for analytic purposes we must have on value / line / song
# We need a unique value per song / society id. We select them randomly
cantometrics_data$dual_id = paste0(cantometrics_data$song_id, 
                                   cantometrics_data$society_id,
                                   cantometrics_data$var_id)

# Sample data. If multiple codings choose the highest value
cantometrics_clean = cantometrics_data %>% 
  group_by(dual_id) %>% 
  arrange(code) %>% 
  slice_tail(n = 1)

x = assert_that(subset(cantometrics_clean, song_id == 1001 & var_id == "line_32")$code == 13)
x = assert_that(all(dim(cantometrics_clean) == c(213314,6)))

# convert Canto data to wide
cantometrics_wide = 
  pivot_wider(cantometrics_clean[,c("song_id", "society_id", "var_id", "code")],
                                values_from = "code", 
                                names_from = "var_id")

# re-scale variables
variable_metadata = read.csv('./raw/gjb/etc/codes.csv')

x = assert_that(sum(is.na(cantometrics_wide[,3:39])) == 398)

# Only keep complete cases
cantometrics_wide = cantometrics_wide[complete.cases(cantometrics_wide),]

## Add society metadata back
cantometrics = dplyr::left_join(cantometrics_wide,
                                cantometrics_societies, 
                                by = "society_id")

## Add song metadata back
# first remove data that exists in both datasets
cantometric_songs = cantometric_songs %>% 
  dplyr::select(-c(Region, Division, Subregion, Area))

cantometrics = dplyr::left_join(cantometrics, cantometric_songs, 
                                by = c("song_id", "society_id"))

# Only keep societies who have complete data (codes for all lines)
complete_data = !is.na(
  rowSums(
    cantometrics[,str_detect(colnames(cantometrics), "line")]
  )
)
cantometrics = cantometrics[complete_data,]

x = assert_that(all(table(cantometrics$song_id) == 1))

cat("CANTOMETRICS SAMPLE:
There are ", nrow(cantometrics), " songs and ", length(unique(cantometrics$society_id)), " societies
    ")

write.csv(cantometrics, 
          'processed_data/cleaned_cantometrics.csv', 
          row.names = FALSE)
