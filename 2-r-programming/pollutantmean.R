pollutantmean <- function(directory, pollutant, id = 1:332) {
  
  files <- (Sys.glob("specdata//*.csv"))[id];
  
  combined_data <- c()
  
  for (file in files) {
    file_data <- read.csv(file, sep = ",");
    pollutant_data <- file_data[,pollutant];
    pollutant_data <- pollutant_data[!is.na(pollutant_data)]
    combined_data <- c(combined_data, pollutant_data)
  }
  
  return(mean(combined_data));
}