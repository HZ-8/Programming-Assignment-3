rankhospital <- function (state, outcome, num = "best") {
    name <- NA
        
    ## Read data
    data <- read.csv ("outcome-of-care-measures.csv", colClasses = "character")
    
    ## Check that state and outcome are valid
    if (!state %in% data$State) {
        stop ("invalid state")
    }
    if (!outcome %in% c("heart attack", "heart failure", "pneumonia")) {
        stop ("invalid outcome")
    }
    
    ## Find column corresponding to the outcome
    outcomes <- c("heart attack" = 11, "heart failure" = 17, "pneumonia" = 23)
    colnum <- outcomes[outcome]
    
    ## Make smaller matrix of rows with my state and no NAs
    index <- (state == data$State) & (!("Not Available" == (data[,colnum])))
    tuned_data <- data[index, c(2, 7, colnum)]
    
    ## Sort by Hospital.Name and then add ranking column
    tuned_data[,3] <- as.numeric(tuned_data[,3])
    tuned_data <- tuned_data[order(tuned_data[,3], tuned_data[,1]),]
    tuned_data <- cbind(tuned_data, c(1:nrow(tuned_data)))
    
    ## Figure out the rank
    if (num == "best") {
        row <- 1
    }
    else if (num == "worst") {
        row <- nrow(tuned_data)
    }
    else {
        row <- num
    }
        
    name <- tuned_data[row, 1]
    name
}