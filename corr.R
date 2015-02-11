corr <- function(directory, threshold = 0) {
  
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
  ## Returns a numeric vector of correlations
  
  ##all files in the directory
  filenames<-list.files(directory,pattern="*.csv", full.names=TRUE)
  ## create an empty column
  correlations <- c()
  
  for (file in filenames) {
    ##reads the files sequentially
    file_data <- read.csv(file, sep = ",");
    ## pulls all the rows that are complete, that doesnt have na. complete.cases function does that
    ##it will put the header as well
    complete_cases <- file_data[complete.cases(file_data),];
    ##if the number of complete cases is more than the threshold specified it will calculate the correlation
    if (nrow(complete_cases) > threshold) {
      correlations <- c(correlations, cor(complete_cases$sulfate, complete_cases$nitrate))
    }
  }
  
  return(correlations)
}
