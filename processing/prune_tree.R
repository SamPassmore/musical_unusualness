### organise jager tree
suppressPackageStartupMessages({
  library(ape)
  library(stringr)
  library(assertthat)
  library(dplyr)
})
  
cantometric_societies = read.csv('processed_data/cleaned_cantometrics.csv') %>% 
  distinct(GlottoID, .keep_all = TRUE)
tree = read.tree("data/world.tre")

language = read.tree("data/world.tre")
taxa_pairing = read.csv('data/taxa.csv')
current_taxa = language$tip.label
stripped_taxa = str_extract(current_taxa, "[^.]+$")

if(all(stripped_taxa == taxa_pairing$taxon)){
  language$tip.label = taxa_pairing$glottocode  
} else{
  cat("Jager tree taxa change failed.")
  stop()
}

tips_tokeep = 
  unique(cantometric_societies$GlottoID)

tips_tokeep= tips_tokeep[
    tips_tokeep %in% 
      unique(taxa_pairing$glottocode)
    ]

language = keep.tip(language, tips_tokeep)

x = assert_that(Ntip(language) == 567)

write.tree(language, file = "processed_data/pruned_tree.tre")
