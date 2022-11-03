## robustness

library(brms)

cantometrics = read.csv('processed_data/cantometrics_modeldata.csv')

fit.full = brm(file = "results/models/full_ri.rds")

cross_val = kfold(fit.full, folds = "stratified", group = "society_id", save_fits = TRUE)
