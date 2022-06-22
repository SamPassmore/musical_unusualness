#' Convert Cantometrics variable scales
#'
#' @param X vector of line values.
#' @param line_set Set of possible values a line can have.
#' @return vector of values on the 0 - 1 scale
#' 

musical_conversion = function(x, line_set){
  matched_df = data.frame(coded_values = line_set, 
                          linear = 1:length(line_set))
  
  x_df = data.frame(coded_values = x)
  
  paired = suppressMessages(
    dplyr::left_join(x_df, matched_df)
  )
  
  (paired$linear - 1) / (max(paired$linear, na.rm = TRUE) - 1)
}
