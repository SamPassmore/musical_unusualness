## Rescale data
suppressPackageStartupMessages({
  library(stringr)
  library(dplyr)
})

source('functions/helper.R')

cantometrics = read.csv("processed_data/cantometrics_wunusualness.csv")
cantometric_codes = read.csv("raw/gjb/etc/codes.csv", fileEncoding="UTF-8-BOM")

# Line indexes
line_idx = str_detect(colnames(cantometrics), "line_")
cols_cantometrics = colnames(cantometrics)[line_idx]
for(i in seq_along(cols_cantometrics)){
  variable_name = cols_cantometrics[i]

  variable = cantometrics[[variable_name]]

  var_set = cantometric_codes %>%
    dplyr::filter(var_id == variable_name) %>%
    pull(code) %>%
    unique(.)

  cantometrics[,paste0(variable_name, "_new")] =
    musical_conversion(variable, var_set)
}

write.csv(cantometrics,
          "processed_data/cantometrics_wunusualness_std.csv")
