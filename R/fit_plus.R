#' Fit a plus model to an abstracts object
#'
#' @param abstracts An abstracts object.
#' @param vocabulary A \code{ext2vec_vocabulary} object from a specific corpus
#' to create a document -  term matrix.
#' @param tf_idf Logical. Default is true, use a tf_idf in the document - term matrix S
#' @param alpha Plus parameter. The elastic net mixing parameter, with \eqn{0\le\alpha\le 1}
#' @param sample_use_time Plus parameter. Number of use time.
#' @param learning_rate Plus parameter. Learning rate. Default is 1.
#' @param qq Plus parameter. quantile threshold

#'
#' @return Return a plus object.(Positive and unlabeled Learning from Unbalanced
#' cases and Sparse structures, PLUS).
#' This is a cross-validated glmnet logistics regression optimized for positive and
#' unlabeled learning. This classification is applied to a dtm matrix created
#' with the vocabulary and the abstracts. The independent variables are the possitive
#' and unknown classes of the provided \code{abstract} object.
#' @export
#'
#' @examples
#' abstracts <- get_abstracts(lacsSample)
#' v <- get_vocabulary(abstracts, term_count = 2)
#' s <- sample(seq(nrow(abstracts)), 65, replace = FALSE)
#' train <- abstracts[s, ]
#' model <- fit_plus(abstracts = train, vocabulary = v)
#' model
fit_plus <- function(abstracts,
                     vocabulary = NULL,
                     tf_idf = TRUE,
                     alpha = 1,
                     sample_use_time = 30,
                     learning_rate = 1,
                     qq = 0.1){

  if(is.null(vocabulary)){
    vocabulary <- get_vocabulary(abstracts)
  }
  x <- get_dtm(abstracts, vocabulary, tf_idf = tf_idf)
  y <- get_y(abstracts)
  loadpkg("plus")
  fit <- plus::plus(x = x,
                    y = y,
                    alpha = alpha,
                    sample_use_time = sample_use_time,
                    learning_rate = learning_rate,
                    qq = qq)
  return(fit)
}

