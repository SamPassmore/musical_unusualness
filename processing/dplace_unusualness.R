## Societal unusualness
suppressPackageStartupMessages({
  library(tidyr)
  library(dplyr)
  library(assertthat)
  library(missForest)
})

source("functions/unusualness_function.R")

ea_variables = read.csv('data/ea_variables.csv')

dplace_long = read.csv('raw/dplace/datasets/EA/data.csv') %>% 
  select(soc_id, var_id, code)
dplace_societies = read.csv('raw/dplace/datasets/EA/societies.csv')
dplace_wide = pivot_wider(data = dplace_long,
                          names_from = var_id, 
                          values_from = code)

# Make sure there is one row per ID
x = assert_that(n_distinct(dplace_wide$soc_id) == nrow(dplace_wide))

dplace_wide = left_join(dplace_wide, dplace_societies,
                        by = c("soc_id" = "id"))

# Match with cantometrics societies
cantometrics_societies = 
  read.csv('raw/gjb/raw/societies.csv')

# get song counts by society
cantometrics_songs = read.csv('raw/gjb/cldf/songs.csv') %>% 
  group_by(society_id) %>% 
  summarise(n_songs = n()) %>% 
  distinct(.keep_all = TRUE)

# Check why metadata isn't the same across these two DB
cantometrics_societies = left_join(cantometrics_societies,
                                   cantometrics_songs,
                                   by = "society_id", 
                                   suffix = c("", ".songs"))
                                   

# How many societies match with DPLACE
dplace_wide = inner_join(dplace_wide, 
                        cantometrics_societies,
                        by = c("xd_id" = "xd_id_1"))

# Ensure there is 1 unique DPLACE society per row
# If DPLACE matches multiple DPLACE societies:
#   choose the one with more songs
dplace_subset = dplace_wide %>% 
  filter(!is.na(n_songs)) %>% 
  group_by(xd_id) %>% 
  slice_max(n_songs, n = 1, with_ties = FALSE)
  
x = assert_that(all(table(dplace_subset$xd_id) == 1))

#### Impute missing data ####
missing_bysociety = 0.25
missing_byvariable = 0.25 

## Subset to societies with less than 25% missing 
society_missingness = rowMeans(is.na(dplace_subset[,ea_variables$id]))
names(society_missingness) = dplace_subset$soc_id
keep_societies = 
  names(society_missingness)[society_missingness < missing_bysociety]

dplace_subset = subset(dplace_subset, subset = soc_id %in% keep_societies)

# Subset to features with less than 25% missing
# for our subset of societies
features_missingness <- apply(dplace_subset, 2, function(x) mean(is.na(x)))
keep_features = 
  names(features_missingness)[features_missingness < missing_byvariable]
dplace_subset = dplace_subset[,keep_features]

### impute the missing data
remaning_eavars = grepl("EA[0-9]{3}", colnames(dplace_subset))
to_impute = dplace_subset[,remaning_eavars] %>% 
  mutate_all(factor) %>% 
  as.data.frame()
dplace_imputed = missForest(xmis = to_impute)

dplace_subset[,remaning_eavars] = dplace_imputed$ximp

## Report matches 
rownames(dplace_subset) = dplace_subset$xd_id
cat(nrow(dplace_subset), 
    "societies matched with DPLACE with",
    sum(dplace_subset$n_songs), "songs\n\n")


### Societal unusualness
rownames(dplace_subset) = dplace_subset$xd_id

all_vars = ea_variables$id[ea_variables$use == 1]
unusualness_allvars = 
  calculate_unusualness(data = dplace_subset,
                        group_var = "Region",
                        variable_set = all_vars,
                        min_group = 5
                          )

kinship_vars = ea_variables$id[ea_variables$kinship == 1]
cat("There are", sum(kinship_vars %in% colnames(dplace_subset)), "kinship variables\n")
unusualness_kinship = calculate_unusualness(data = dplace_subset,
                       group_var = "Region",
                       variable_set = kinship_vars,
                       min_group = 5)


economy_vars = ea_variables$id[ea_variables$economy == 1]
cat("There are", sum(economy_vars %in% colnames(dplace_subset)), "economic variables\n")
unusualness_economy = calculate_unusualness(data = dplace_subset,
                                            group_var = "Region",
                                            variable_set = economy_vars,
                                            min_group = 5)

housing_vars = ea_variables$id[ea_variables$housing == 1]
cat("There are",sum(housing_vars %in% colnames(dplace_subset)), "housing variables\n")
unusualness_housing = calculate_unusualness(data = dplace_subset,
                                            group_var = "Region",
                                            variable_set = housing_vars,
                                            min_group = 5)

society_unusualness = cbind(xd_id = dplace_subset$xd_id,
                    u_ea = unusualness_allvars,
                    u_kinship = unusualness_kinship,
                    u_economy = unusualness_economy,
                    u_housing = unusualness_housing
                    )

cantometrics_dplace = left_join(
  data.frame(society_unusualness),
  cantometrics_societies,
  by = c("xd_id" = "xd_id_1"))

cantometrics_dplace = cantometrics_dplace %>% 
  select(xd_id, society_id, u_ea, u_kinship, u_economy, u_housing)

x = assert_that(all(table(cantometrics_dplace$society_id) == 1))

write.csv(cantometrics_dplace, "processed_data/dplace_unusualness.csv")
