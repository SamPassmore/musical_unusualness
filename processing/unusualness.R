# Unusualness
suppressPackageStartupMessages({
  library(stringr)
  library(purrr)
  library(dplyr)
  library(ape)
})

cantometrics = read.csv('processed_data/cleaned_cantometrics.csv')

line_idx = str_detect(colnames(cantometrics), "line_")

## Subset to all societies with a taxa 
# pruned_tree = read.tree('processed_data/pruned_tree.tre')
# cantometrics = cantometrics %>% 
#   dplyr::filter(cantometrics$GlottoID %in% pruned_tree$tip.label)

#### Calculate state probabilities ####
state_probabilities = apply(
  cantometrics[,line_idx],
  2,
  function(x){
    prop.table(table(x))
  }
)

#### Calculate Unusualness Score Across the whole sample ####
lines = colnames(cantometrics)[line_idx]
unusualness = sapply(lines, function(l){
  log(state_probabilities[[l]][as.character(cantometrics[,l])]) 
  }) %>%
  rowSums()

cantometrics$unusualness_wholesample = unusualness

#### Calculate Unusualness Score by MacroArea ####

cat("Calculate Unusualness by Region and society LOO...\n")

#### Calculate Unusualness Score by MacroArea ####
regions = unique(cantometrics$Region)
# remove NA regions
regions = regions[!is.na(regions)]

regional_unusualness_loo = rep(NA, nrow(cantometrics))
names(regional_unusualness_loo) = cantometrics$song_id

for(i in seq_along(regions)){
  cat("Starting", regions[i], "\n")
  
  region_cantometrics = cantometrics %>% 
    dplyr::filter(Region == regions[i])
  
  song_ids = region_cantometrics$song_id
  
  for(j in song_ids){
    # this song comes from which society
    society_id = region_cantometrics$society_id[region_cantometrics$song_id == j]
    r_cantometrics = region_cantometrics[!region_cantometrics$society_id %in% society_id,]
    song = region_cantometrics[region_cantometrics$song_id %in% j,]
    regional_stateprobabilities = apply(r_cantometrics[,line_idx],
                                        2,
                                        function(x){
                                          prop.table(table(x))
                                        }
    )
    
    song_unusualness = sapply(lines, function(l){
      log(regional_stateprobabilities[[l]][
        as.character(song[,l])]) 
    }) %>% sum()
    
    if(length(regional_unusualness_loo) != 5484){stop()}
    
    regional_unusualness_loo[as.character(j)] = song_unusualness
  }
}


#### Calculate Unusualness Score by Language family ####
language_families = unique(cantometrics$FamilyLevGlottocode)
language_families = language_families[!is.na(language_families)]

languagefamily_unusualness = rep(NA, nrow(cantometrics))
names(languagefamily_unusualness) = cantometrics$song_id

cat("Calculate Unusualness by Language family...\n")

for(i in seq_along(language_families)){
  
  lf_cantometrics = cantometrics %>% 
    dplyr::filter(FamilyLevGlottocode == language_families[i])
  
  n_societies = lf_cantometrics %>% 
    pull(society_id) %>% 
    n_distinct()
  
  if(n_societies == 1){
    # If we only have a language isolate or only one society from
    # a family it doesnt make sense to calculate unusualness
    # So follow Skirgård et al and use regional scores 
    id = lf_cantometrics$song_id
    languagefamily_unusualness[as.character(id)] = 
      regional_unusualness_loo[as.character(id)]
  } else {
    lf_stateprobabilities = apply(
      lf_cantometrics[,line_idx],
      2,
      function(x){
        prop.table(table(x))
      }
    )
    
    lf_unusualness = rowSums(
      sapply(lines, 
                            function(l){
      log(lf_stateprobabilities[[l]]
          [as.character(lf_cantometrics[,l])]) 
    }))
    names(lf_unusualness) = lf_cantometrics$song_id
    languagefamily_unusualness[
      match(names(lf_unusualness), 
            names(languagefamily_unusualness))] =
      lf_unusualness 
  }
}

## Add unusualness to dataset
cantometrics$unusualness_region[
  match(names(regional_unusualness_loo), 
        cantometrics$song_id)
] = regional_unusualness_loo

cantometrics$unusualness_languagefamily[
  match(names(languagefamily_unusualness), 
        cantometrics$song_id)
  ] = languagefamily_unusualness

write.csv(cantometrics,
          'processed_data/cantometrics_wunusualness.csv')
