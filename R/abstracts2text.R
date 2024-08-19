#' Validate a data frame with abstracts to construct vectpr of strings
#'
#' @param abstracts An object of the class abstracts (a data frame with abstracts) or
#'a string vector with abstracts
#'
#' @return A validated \code{character} vector. The function check and encode characters
#' in utf8 standard.
#' @export
#'
#' @examples
#' abstracts <- get_abstracts(lacsSample)
#' abstracts2text(abstracts)
abstracts2text <- function(abstracts = NULL){
  if(is.null(abstracts)){
    return(character())
  } else if(any(is(abstracts) %in% "abstracts")){
    text <- abstracts$abstract
  } else if(is.character(abstracts)){
    text <- abstracts
  } else{
    stop("Provide an abstracts object or a character vector")
  }
  text  <- utf8::utf8_encode(text)
  text <- utf8::utf8_format(text)
  return(text)
}
