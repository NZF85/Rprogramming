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
  
  ##all files in the directory
  filenames<-list.files(directory,pattern="*.csv", full.names=TRUE)
  ## create an empty column
  nobs <- c();
  
  for (index in id) {
    ## reads the files by the index
    file_data <- read.csv(filenames[index], sep = ",");
    ## pulls all the rows that are complete, that doesnt have na. complete.cases function does that
    complete_cases <- file_data[complete.cases(file_data),];
    ##nrow counts the number of rows in the matrix. then we put the latest entry below the previous nobs
    nobs <- c(nobs, nrow(complete_cases));
  }
  ##id comes in a vector e.g. 1,2,3.. so our nobs corresponds to the id and we bind them together
  return(data.frame(cbind(id, nobs)));
}
