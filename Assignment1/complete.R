complete <- function(directory, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return a data frame of the form:
  ## id nobs
  ## 1  117
  ## 2  1041
  ## ...
  ## where 'id' is the monitor ID number and 'nobs' is the
  ## number of complete cases
  
  files <- list.files(directory, full.names=TRUE)
  result <- data.frame(id = id, nobs = id)
  for(i in id){
    csv <- read.csv(paste(directory, "/", sprintf("%03d", i), ".csv", sep=''))
    for(j in 1:ncol(csv)){
       csv <- csv[!is.na(csv[,j]),]
    }
    result[result$id==i,2] = nrow(csv)
  }
  result
}