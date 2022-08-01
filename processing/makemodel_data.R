# Make model data
suppressPackageStartupMessages({
  library(dplyr)
  library(geosphere)
  library(ggplot2)
  library(ape)
})

min_songs = 5

cantometrics = read.csv("processed_data/cantometrics_wunusualness.csv") %>% 
  # Only analyse societies with more than 5 songs
  add_count(society_id, name = "n_songs") %>% 
  dplyr::filter(n_songs >= min_songs)

## Get the difference between the society average value and the 
## language family average value to use as a predictor
cantometrics = cantometrics %>% 
  # First get average value by language family
  group_by(Region) %>% 
  mutate(regional_mean = mean(unusualness_region)) %>% 
  ungroup(Region) %>% 
  # then calculate the average per society and take the difference
  # from the language family 
  group_by(society_id) %>% 
  mutate(society_mean = mean(unusualness_region),
         society_region_diff = society_mean - regional_mean)

## Calculate the societal score of the nearest geographic neighbor
cantometrics_societies = read.csv("raw/gjb/cldf/societies.csv") %>% 
  dplyr::filter(!is.na(Society_longitude) & !is.na(Society_latitude))

# Some socities have the same Lat/Lon. We jitter so they are not in 
# exactly the same spot. 
cantometrics_societies$Society_latitude = 
  jitter(cantometrics_societies$Society_latitude, 
         amount = 0.0005)

# Caclulate the distance between all Cantometric societies
cantometrics_geographicdistance = 
  distm(x = cantometrics_societies[,c("Society_longitude",
                                      "Society_latitude")]) / 1000

# Make all diagonals NA - otherwise they are the smallest distance
diag(cantometrics_geographicdistance) = NA

dimnames(cantometrics_geographicdistance) = 
  list(cantometrics_societies$society_id,
       cantometrics_societies$society_id)

# For each society calculate the smallest distance
nearest_neighbour =
  data.frame(
    society_id = cantometrics_societies$society_id,
    nn_distance_km = 
      apply(cantometrics_geographicdistance, 2, min, na.rm = TRUE) 
  )

cantometrics = left_join(cantometrics, nearest_neighbour,
                         by = "society_id")  

## For each society, count the number of societies 
## within x kilometers 
min_dist = 250
n_cantoneighbour_250 = apply(cantometrics_geographicdistance, 2,
                               function(x) sum(x < min_dist, na.rm = TRUE)
)

min_dist = 500
n_cantoneighbour_500 = apply(cantometrics_geographicdistance, 2,
                             function(x) sum(x < min_dist, na.rm = TRUE)
)

min_dist = 1000
n_cantoneighbour_1000 = apply(cantometrics_geographicdistance, 2,
                             function(x) sum(x < min_dist, na.rm = TRUE)
)

n_neighbours =
  data.frame(
    society_id = cantometrics_societies$society_id,
    glottocode = cantometrics_societies$GlottoID,
    n_neighbours_250 = n_cantoneighbour_250,
    n_neighbours_500 = n_cantoneighbour_500,
    n_neighbours_1000 = n_cantoneighbour_1000
    )

cantometrics = left_join(cantometrics, n_neighbours,
                         by = "society_id") 

cantometrics = left_join(cantometrics, glottolog,
                         by  =c("GlottoID" = "Glottocode"))

#### Get nearest phylogenetic neighbour ####
tree = read.tree('processed_data/pruned_tree.tre')

tree_distance = cophenetic.phylo(tree)
# make diagonal NA so we don't pick it as min
diag(tree_distance) = NA

min_distance = apply(tree_distance, 2, min, na.rm = TRUE) 

phylodistance_df = data.frame(GlottoID = names(min_distance),
                              nearest_phyloneighbour = min_distance)

cantometrics = left_join(cantometrics, 
                         phylodistance_df,
                         by  = "GlottoID")


### Add DPLACE unusualness
dplace_u = read.csv('processed_data/dplace_unusualness.csv')

cantometrics = left_join(cantometrics, 
                         dplace_u,
                         by = "society_id")

## Keep only complete cases
cantometrics = cantometrics %>% 
  dplyr::select(song_id, society_id,
                unusualness_wholesample, 
                unusualness_region,
                unusualness_languagefamily,
                regional_mean,
                society_mean, 
                society_region_diff,
                nn_distance_km, nearest_phyloneighbour,
                n_neighbours, n_glottoneighbours, 
                u_ea, u_kinship, u_economy, u_housing,
                GlottoID, FamilyLevGlottocode, 
                Region,
                Society_latitude)


write.csv(cantometrics, 
          file = "processed_data/cantometrics_modeldata.csv",
          row.names = FALSE)
