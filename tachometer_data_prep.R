library(data.table)
library(dplyr)


calculate_col_mean <- function(data, col_name) {
  col_mean <- mean((data[, col_name]), na.rm = TRUE)
  return(col_mean)
}

calculate_col_mean_with_filter <- function(data, col_name, filters) {
  filtered_data <- data
  for (filter in filters) {
    filter_name <- filter[[1]]
    filter_value <- filter[[2]]
    filtered_data <- dplyr::filter(filtered_data, !!sym(filter_name) == filter_value)
  }

  filtered_data_mean <- calculate_col_mean(filtered_data, col_name)
  return(filtered_data_mean)
}
