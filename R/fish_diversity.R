#' Compute Fish Diversity in given data set
#' 
#' This code requires a vector containing fish types and returns the total number of fish, the most common species and the least common species
#' 
#' @param fish vector of fish
#'
#' @return list with the following items
#' ' \describe{
#' \item{total}{ Number of fish}
#' \item{rare}{Name of fish least frequently occuring fish}
#' \item{dominant}{Name of the most frequently occuring species}
#' 


fish_diversity = function(fish) {


    # find total number of fish
    total = nrow(fish)
    # determine which fish has greatest n()
    dominant = names(which.max(table(fish$type)))
    # determine fish with lowest n()
    rare = names(which.min(table(fish$type)))
    
    # create text output to explain what each value is 
    total_fish = sprintf("There is a total of %s fish. ", total)
    most_common = sprintf("The most common fish is %s ", dominant)
    least_common = sprintf("and the least common fish is %s. ", rare)
    total
  
    #return(list(total, dominant, rare)
    message(total_fish, most_common, least_common)
}