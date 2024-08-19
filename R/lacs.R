#' Create an object of class lacs
#'
#' @param abstracts Object of \code{abstracts} class. A dataset of abstracts
#' @param vocabulary A vocabulary created with the \code{get_vocabulary} function, from
#' an \code{abstracts} object
#' @param term_count_min Vocabulary parameter. The minimum number of counts for an specific term in vocabulary
#' @param doc_proportion_min 	Vocabulary parameter. The minimum proportion of documents which should contain term
#' @param doc_proportion_max The Vocabulary parameter. The maximum proportion of documents which should contain term.
#' @param alpha Plus parameter. The elastic net mixing parameter, with \eqn{0\le\alpha\le 1}
#' @param sample_use_time Plus parameter. Number of use time.
#' @param learning_rate Plus parameter. Learning rate. Default is 1.
#' @param qq Plus parameter. quantile threshold

#' @return An object of class lacs
#' @export
#'
#' @examples
#' lacs(lacsSample, tail(example_vocabulary, 500))
lacs <- function(abstracts,
                 vocabulary = NULL,
                 term_count_min = 2,
                 doc_proportion_min = 0,
                 doc_proportion_max = 1,
                 alpha = 1,
                 sample_use_time = 30,
                 learning_rate = 1,
                 qq = 0.1){
  this.call <- match.call()

  abstracts <- get_abstracts(abstracts)

  if(is.null(vocabulary)){
    vocabulary <- get_vocabulary(abstracts, term_count_min = term_count_min, doc_proportion_min = doc_proportion_min, doc_proportion_max = doc_proportion_max )
  }

  plus <- fit_plus(abstracts,
                   vocabulary,
                   alpha = alpha,
                   sample_use_time = sample_use_time,
                   learning_rate = learning_rate,
                   qq = qq)

  lacs <- new_lacs(plus = plus,
           vocabulary = vocabulary,
           train = abstracts,
           call = this.call)
  return(lacs)
}
