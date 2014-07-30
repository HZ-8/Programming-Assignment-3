best <- function (state, outcome) {
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
    
    ## Sort by Hospital.Name and then find minimun in this table
    tuned_data <- tuned_data[order(tuned_data[,2]),]
    tuned_data[,3] <- as.numeric(tuned_data[,3])
    row <- which.min(tuned_data[,3])
    name <- tuned_data[row, 1]
    name
}