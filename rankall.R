rankall <- function(outcome = character(), num = "best")
{
  if(num =="best"){num <- 1}
  ##Reading outcome data
  data <- read.csv("outcome-of-care-measures.csv",  na.strings = "Not Available", stringsAsFactors = FALSE)
  outcomes <- c("heart attack" = 11, "heart failure" = 17, "pneumonia" = 23)
  check_data <- data[ ,c(2,7,outcomes[outcome])]
  names(check_data) <- c("Hospital", "State", "Outcome")
  library(plyr)
  arranged_data <- arrange(check_data, State, Outcome, Hospital)
  arranged_data <- na.omit(arranged_data)
  arranged_data <- arranged_data[ , c(1,2)]
  final_data <- split(arranged_data, arranged_data$State)
  hospitalname <- function(x = list(), n=num){
    if(n=="worst"){
      n <- nrow(x)
      x$Hospital[n]
    }else{
      x$Hospital[n]
    }
  }
  ##Checking validity of state and outcome
  if(outcome %in% names(outcomes))
  {
    my_list <- sapply(final_data, hospitalname)
    states <- names(my_list)
  }else
  {
    stop("invalid outcome")
  }
  my_data <- data.frame(hospital=my_list, state=states, row.names = states)
}