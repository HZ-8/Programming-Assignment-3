rankall <- function (outcome, num = "best") {
    # Initialize blank result to return
    h_list <- data.frame("hospital" = character(), "state" = character())
    
    ## Read data
    data <- read.csv ("outcome-of-care-measures.csv", colClasses = "character")
    
    ## Check that outcome is valid
    if (!outcome %in% c("heart attack", "heart failure", "pneumonia")) {
        stop ("invalid outcome")
    }
    
    ## Find column corresponding to the outcome
    outcomes <- c("heart attack" = 11, "heart failure" = 17, "pneumonia" = 23)
    colnum <- outcomes[outcome]


    ## Make smaller matrix of rows with no NAs and cols we need
    row_index <- !("Not Available" == (data[,colnum]))
    col_index <- c("Hospital.Name", "State", names(data)[colnum])
    tuned_data <- data[row_index, col_index]
    colnames(tuned_data) <- c("Hospital", "State", "Rate")  
    
    ## Figure out the rank
    if (num %in% c("best", "worst")) {
        row <- 1
    }
    else {
        row <- num
    }
    
    ## Sort by State, Hospital Name and Rate; then split by State
    tuned_data$Rate <- as.numeric(tuned_data$Rate)
   
    if (!num == "worst") {
        index <- order(tuned_data$State, tuned_data$Rate, tuned_data$Hospital)
    }
    else {
        index <- order(tuned_data$State, -tuned_data$Rate, tuned_data$Hospital)
    }
    
    tuned_data <- split(tuned_data[index,], tuned_data[index,]$State)
    
    ## For each state, get the hospital name and add it to result
    for (i in seq_along(tuned_data)) {
        block <- tuned_data[[i]]
        rownames(block) <- NULL
        name <- block$Hospital[row]
        state <- block$State[1]
        h_list <- rbind(h_list, data.frame("hospital" = name, "state" = state))
    }
    
    h_list
}
