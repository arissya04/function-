detect_missing_values <- function(data) {
  missing_counts <- numeric(ncol(data))
  names(missing_counts) <- colnames(data)
  
  for(i in 1:ncol(data)) {
    missing_counts[i] <- sum(is.na(data[[i]]))
  } 
  missing_counts <- missing_counts[missing_counts > 0]
  return(missing_counts)
}
sample_data <- data.frame(
  Height = c(150, 175, 178, NA, 165,160 , 180, 160),
  Weight = c(70, NA, 60, 45, NA, 110, 85, 65),
  Blood_Pressure = c(110, 120, NA, 126, 115, NA, 98, 126)
)

detect_missing_values(sample_data)

median_impute <- function(x){
  x [is.na(x)] <- median (x, na.rm = TRUE)
  return(x)
}


sample_data <- sapply(sample_data,median_impute)
print(sample_data)