#' abstracts model helper function
#'
#' @param x A data frame with abstracts. Should contain doi, title, abstracts and a class column.
#' The class should be possitive or unknown.
#' @return and object of the class abstracts
#' @export
#'
#' @examples
#' abstracts <- get_abstracts(lacsSample)
get_abstracts <- function(x = NULL){

  abstracts <- validate_abstracts(x)
  abstracts <- new_abstracts(doi = abstracts$doi,
                title = abstracts$title,
                abstract = abstracts$abstract,
                class = abstracts$class)
  return(abstracts)
}
