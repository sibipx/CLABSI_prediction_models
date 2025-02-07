#' Creates catheter episodes (catheter schemes) based on a vector of start dates
#' and a vector of end dates. Expects vectors ordered by start_time
#' 
#' @param start_time vector of start dates
#' @param end_time vector of end dates
#' 

group_overlap <- function(start_time, end_time){
  
  # check vectors equal
  if (length(start_time) != length(end_time)) stop("Vectors should be of same length")
  # check that there are no NA's
  if (any(is.na(start_time)) | any(is.na(end_time))) stop("Vectors should not have NA values")
  # check that start_time is sorted in ascending order
  if(!all(sort(start_time, decreasing = FALSE) == start_time)) stop("First vector should be sorted in ascending order")
  
  # if empty vector return empty vector
  if (length(start_time) == 0) return(c(0)) 
  # if there is only 1 row, return group = 1 (gives better performance)
  if (length(start_time) == 1) return(c(1)) 
  
  # initialize group 1
  group_interval <- c(start_time[[1]], end_time[[1]])
  group_number <- rep(1, length(start_time))
  
  for (i in 2:length(start_time)){
    # if current start time is in the group interval
    if (group_interval[[1]] <= start_time[i] && start_time[i] <= group_interval[[2]]){
      group_number[i] <- group_number[i-1] # preserve the previous group
      group_interval <- c(group_interval[[1]], max(group_interval[[2]], end_time[i])) # set the group interval to max (end_time)
    } else { # start a new group, incrementing previous group
      group_number[i] <- group_number[i-1] + 1
      group_interval <- c(start_time[i], end_time[i])
    }
    
  }
  
  return(group_number)
  
}
