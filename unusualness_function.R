calculate_unusualness = function(data, group_var, variable_set, 
                                 min_group = 10, impute = FALSE){
  require(dplyr)
  require(tibble)
  if(missing(group_var)){
    ## Code to make the function evaluate all data without groups
  } else {
    g = unlist(
      unique(data[,group_var])
      )
  }
  
  if(is.null(rownames(data))){
    stop("Data needs rownames")
  }
  
  unusualness = rep(NA, nrow(data))
  names(unusualness) = rownames(data)
  for(group in g){
    # subset to group
    d = data %>% 
      rownames_to_column("id") %>% 
      dplyr::filter(.data[[group_var]] == group) %>% 
      data.frame(.)
    rownames(d) = d$id
    
    if(nrow(d) <= min_group) next
    
    # calculate group state probabilities for the variable set
    state_probabilities = apply(
      d[,variable_set], 2, function(x){prop.table(table(x))}
      )
    
    
    if(impute){
      # If a code is missing replace it with the group mode
      d[,variable_set] = d[,variable_set] %>% 
        mutate_all(~ifelse(is.na(.x), 
                           Mode(.x), .x))              
      }
    
    # Replace codes with state probabilities
    unusualness_byvariable = sapply(variable_set, function(vs){
      log(state_probabilities[[vs]][as.character(unlist(d[,vs]))])
    }) 
    
    # Sum by song to calculate unusualness
    unu = rowSums(unusualness_byvariable)
    names(unu) = rownames(d)
    
    unusualness[match(names(unu), names(unusualness))] = unu
  }
    
  unusualness
}

Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
