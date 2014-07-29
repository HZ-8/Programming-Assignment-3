best <- function (state, outcome) {
    data <- read.csv ("outcome-of-care-measures.csv", colClasses = "character")
    
    if (sum(state == data$State) == 0) {
        stop ("invalid state")
    }
    if (sum(outcome == c("heart attack", "heart failure", "pneumonia")) == 0) {
        stop ("invalid outcome")
    }
}