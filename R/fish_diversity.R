#' Compute Fish Diversity in given data set
#' 
#' purpose
#' 
#' @param fish vector of fish
#'
#' @return list with the following items
#' ' \describe{
#' \item{total}{ Number of fish}
#' \item{rare}{Name of fish least frequently occuring fish}
#' \item{dominant}{Name of the most frequently occuring species}
#' 


fish_test = function(fish) {
  fish_list <- fish %>% 
    group_by(type)
    summarize(count = n()) %>% 

    
  total = length(summary(fish))
  
  dominant = names(which.max(table(fish$type)))
  
    # find total number of fish
    total = length(fish)
    # determine which fish has greatest n()

    # determine fish with lowest n()
    rare= names(which.min(table(fish$type)))
    
  
  return(list(total=total,dominiant=dominant, rare=rare))
}