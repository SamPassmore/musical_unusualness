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
pruned_tree = read.tree('processed_data/pruned_tree.tre')
cantometrics = cantometrics %>% 
  dplyr::filter(cantometrics$GlottoID %in% pruned_tree$tip.label)

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
regions = unique(cantometrics$Region)

regional_unusualness = rep(NA, nrow(cantometrics))
names(regional_unusualness) = cantometrics$song_id

cat("Calculate Unusualness by Region...\n")

for(i in seq_along(regions)){
  
  region_cantometrics = cantometrics %>% 
    dplyr::filter(Region == regions[i])
  
  n_societies = region_cantometrics %>% 
    pull(society_id) %>% 
    n_distinct()
  
  regional_stateprobabilities = apply(
    region_cantometrics[,line_idx],
    2,
    function(x){
      prop.table(table(x))
    }
  )
  
  unusualness_byregion = sapply(lines, function(l){
    log(regional_stateprobabilities[[l]][
      as.character(region_cantometrics[,l])]) 
  }) %>%
    rowSums()
  names(unusualness_byregion) = region_cantometrics$song_id
  regional_unusualness[
    match(names(unusualness_byregion), 
          names(regional_unusualness))] =
    unusualness_byregion
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
      regional_unusualness[as.character(id)]
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
  match(names(regional_unusualness), 
        cantometrics$song_id)
] = regional_unusualness

cantometrics$unusualness_languagefamily[
  match(names(languagefamily_unusualness), 
        cantometrics$song_id)
  ] = languagefamily_unusualness

write.csv(cantometrics,
          'processed_data/cantometrics_wunusualness.csv')
