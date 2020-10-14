rankhospital <- function(state = character(), outcome = character(), num = "best")
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
  final_data <- split(arranged_data, arranged_data$State)
  if(num == "worst"){num <- nrow(final_data[[state]])}
  ##Checking validity of state and outcome
  all_states <- arranged_data[ , 2]
  if(state %in% all_states && outcome %in% names(outcomes))
  {
    final_data[[state]][[1]][[num]]
  }else if(state %in% all_states == FALSE)
  {
    print("Invalid state")
  }else if(outcome %in% names(outcomes) == FALSE)
  {
    print("Invalid outcome")
  }
}