corr <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
  ## Return a numeric vector of correlations
  result <- complete(directory)
  num <- NA

  for(i in 1:nrow(result)){
    if(result[i,2] > threshold){
      csv <- read.csv(paste(directory, "/", sprintf("%03d", i), ".csv", sep=''))
      for(j in 1:ncol(csv)){
        csv <- csv[!is.na(csv[,j]),]
      }
      num <- cbind(num,cor(csv[,2],csv[,3]))
    }
  }
  num[!is.na(num)]
}