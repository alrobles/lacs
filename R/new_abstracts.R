#' Constructor function for the class abstracts
#' A constructor for the abstracts class
#'
#' @param doi a vector with DOI for each paper
#' @param title A vector of titles for each paper
#' @param abstract A vector of abstracts for each paper
#' @param class A vector with the class for each paper
#'
#' @export
#' @return an object of class abstracts. Natively is a data.frame
#'
#' @examples
#' new_abstracts(
#' doi = example_doi,
#' title = example_title,
#' abstract = example_abstract,
#' class = example_class
#' )
new_abstracts <- function(doi = character(),
                     title = character(),
                     abstract = character(),
                     class = character()){
  abstracts <- tibble::tibble(doi = doi,
         title = title,
         abstract = abstract,
         class = class)

  class(abstracts) <- c("abstracts", class(abstracts))
  return(abstracts)
}
