rankhospital <- function(state, outcome, num = "best") {
        ## Read outcome data
        data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        ## Check that state and outcome are valid
        flag <- FALSE
        collect.state <- c(unique(data[, 7]))
        for(i in collect.state){
                if(i == state){
                        flag = TRUE
                        break
                }
        }
        if(flag == FALSE){
                stop("invalid state")
        }
        
        flag <- FALSE
        collect.outcome <- c("heart attack", "heart failure", "pneumonia")
        for(i in collect.outcome){
                if(i == outcome){
                        flag = TRUE
                        break
                }
        }
        if(flag == FALSE){
                stop("invalid outcome")
        }
        ## Return hospital name in that state with the given rank
        ## 30-day death rate
        ## Select area
        data.area <- data[data$State == state, ]
        
        ## Select data aviliable
        col <- 0
        if(outcome == "heart attack"){
                col <- 11
        } else if(outcome == "heart failure"){
                col <- 17
        } else{
                col <- 23
        }
        data.aviliable <- data.area[data.area[, col] != "Not Available", ]
        ##  define row
        row <- 0
        if(num == "best"){
                row <- 1
        } else if(num == "worst"){
                row <- nrow(data.aviliable)
        } else{
                row <- as.numeric(num)
        }
        if(row > nrow(data.aviliable)){
                c(NA)
        } else{
                data.aviliable <- data.aviliable[order(as.numeric(data.aviliable[,col]), data.aviliable[,2]),]
                data.aviliable[row, 2]
        }
        
}