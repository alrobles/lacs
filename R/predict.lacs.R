#' Predicted classification with a lacs model
#' @aliases predict.lacs
#' @param object An object of lacs class to predict the text classification
#' @param abstracts An object of class abstracts. Is a \code{data.frame} with
#'   abstracts
#' @param type The type of the predicted response given a lacs model. Could be
#'   the class (Binary output) or the probability given the plus model inside
#'   the lacs object
#' @param \dots additional arguments
#' @param plus If \code{TRUE} use the cutoff threshold to predict the class from
#' the plus object. If \code{FALSE} use the \code{cv.glmnet} object without
#' cutoff threshold.
#'
#' @return A vector of response. If the type is set as class returns a logical
#'   vector. If the type is response return a vector of probabilities.
#' @rdname predict
#' @export predict
predict <- function(object, ...) {
  UseMethod("predict")
}
#' @rdname predict
#' @export predict.lacs
#' @export
#'
#' @examples
#' model <- lacs(lacsSample[1:200, ])
#' lacsSample <- get_abstracts(lacsSample)
#' predict(model, abstracts = lacsSample )
#' predict(model, abstracts = lacsSample,  plus = FALSE )
#' predict(model, abstracts = lacsSample, type = "response")
predict.lacs <- function(object, ..., abstracts, type = "class", plus = TRUE){
  text <- abstracts2text(abstracts)

  newx <- get_dtm(text, object$vocabulary)

  if(type == "class"){
    if(plus){
      y <- stats::predict(object$plus, newx = newx, type = "class")
    } else {
      y <- stats::predict(object$plus$plus, newx = newx, type = "class")
      y <- as.numeric(y)
    }
  } else if(type == "response"){
    if(plus){
      y <- stats::predict(object$plus, newx = newx, type = "response")
    } else {
      y <- stats::predict(object$plus$plus, newx = newx, type = "response")
    }

  }
  return(y)
}
