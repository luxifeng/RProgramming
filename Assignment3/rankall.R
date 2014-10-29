rankall <- function(outcome, num = "best") {
        ## Read outcome data
        data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        
        ## Check that state and outcome are valid
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
        
        ## Define column
        col <- 0
        if(outcome == "heart attack"){
                col <- 11
        } else if(outcome == "heart failure"){
                col <- 17
        } else{
                col <- 23
        }
        
        ## For each state, find the hospital of the given rank
        collect.state <- c(unique(data[, 7]))
        collect.state <- collect.state[order(collect.state)]
        collect.result <- NULL
        for(state in collect.state){
                collect.result <- rbind(collect.result, insiderankhospital(data, state, col, num))
        }
        ## Return a data frame with the hospital names and the
        ## (abbreviated) state name
        collect.result <- cbind(collect.result, collect.state)
        ## Build data.frame and name rows and column
        result <- data.frame(collect.result)
        rownames(result) <- collect.state
        colnames(result) <- c('hospital', 'state')
        result
}

insiderankhospital <- function(data, state, col, num){
        ## Select area
        data.area <- data[data$State == state, ]
        
        ## Select data aviliable
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