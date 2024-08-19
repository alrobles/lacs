#' Get a document term matrix from abstracts
#'
#' @param abstracts Could be an object of class abstracts (created via \code{get_abstracts} function) or
#' a \code{character}vector with abstracts
#' @param vocabulary A vocabulary created with the \code{get_vocabulary} function, from
#' an \code{abstracts} object
#' @param tf_idf logical. If it is true returns dtm weighted with tf idf. Otherwise return
#' binary dtm matrix
#'
#' @return Returns a document term matrix. Each row is a document, each term is a matrix from
#' vocabulary
#' @export
#'
#' @examples
#' abstracts <- get_abstracts(lacsSample)
#' vocabulary <- get_vocabulary(abstracts, term_count = 10)
#' get_dtm(abstracts, vocabulary, tf_idf = FALSE)
get_dtm <- function(abstracts, vocabulary, tf_idf = TRUE){

  if(any(is(abstracts) %in% "abstracts")){
    text <- abstracts$abstract
  } else if(is.character(abstracts)){
    text <- abstracts
  } else{
    stop("Provide an abstracts object or a character vector")
  }
  text  <- utf8::utf8_encode(text)
  prep_fun <- tolower

  it <-  text2vec::itoken(text,
                              preprocessor = prep_fun,
                              progressbar = FALSE)

  vectorizer <- text2vec::vocab_vectorizer(vocabulary = vocabulary)
  dtm  <-  text2vec::create_dtm(it = it, vectorizer = vectorizer)

  if(tf_idf){
    tfidf = text2vec::TfIdf$new()
    dtm <- text2vec::fit_transform(dtm, tfidf)
  }
  return(dtm)
}
