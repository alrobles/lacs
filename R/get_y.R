#' Get the independent variable for an abstracts object
#'
#' @param abstracts An object of class abstracts is a data frame
#' with a class column. Shoiuld have possitive and unknown classes
#'
#' @return A vector of binary output
#' @export
#'
#' @examples
#' abstracts <- get_abstracts(lacsSample)
#' get_y(abstracts)
get_y <- function(abstracts){
  if(is(abstracts) == "abstracts"){
    classes <- abstracts$class
  } else{
    stop("Provide an abstracts")
  }
  return(as.numeric(classes == "possitive") )
}
