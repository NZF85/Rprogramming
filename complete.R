complete <- function(directory, id = 1:332) {
  
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Returns a data frame of the form:
  ## id nobs
  ## 1  117
  ## 2  1041
  ## ...
  ## where 'id' is the monitor ID number and 'nobs' is the
  ## number of complete cases
  
  filenames<-list.files(directory,pattern="*.csv", full.names=TRUE)
  nobs <- c();
  
  for (index in id) {
    file_data <- read.csv(filenames[index], sep = ",");
    complete_cases <- file_data[complete.cases(file_data),];
    nobs <- c(nobs, nrow(complete_cases));
  }
  
  return(data.frame(cbind(id, nobs)));
}
