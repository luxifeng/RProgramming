best <- function(state, outcome) {
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
        
        ## Return hospital name in that state with lowest 30-day death
        ## rate
        ## Select Area
        data.area <- data[data$State == state, ]
        ## Select data ava
        col <- 0
        if(outcome == "heart attack"){
                col <- 11
        } else if(outcome == "heart failure"){
                col <- 17
        } else{
                col <- 23
        }
        data.aviliable <- data.area[data.area[, col] != "Not Available", ]
        ## Select min data
        rate <- as.numeric(data.aviliable[, col])
        range(rate, na.rm = TRUE)
        row <- which.min(rate)

        data.aviliable[row, 2]
}