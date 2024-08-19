
#' Title
#'
#' @param object A lacs object
#' @param abstracts An object of class abstracts. Is a \code{data.frame} with
#'   abstracts
#' @param plus Logical. If it is true use the cutoff threshold from the plus model.
#' This tends to reduce the false negatives and increase the recall. Othewise
#' gets the predicted value from glmnet default predicted response.
#' @param \dots additional arguments
#' @return A \code{data.frame} containing four columns. The truth independent
#' value (observed y), the probability to belong in Class1, the probability to
#' belong Class2 and the predicted class either the plus (use cutoff threshold)
#' or glmnet model.
#' @export
#'
#' @rdname predictions
#' @export predictions
predictions <- function(object, ...) {
  UseMethod("predictions")
}
#' @rdname predictions
#' @export predictions.lacs
#' @export
#'
#' @examples
#' model <- lacs(lacsSample[1:200, ])
#' lacsSample <- get_abstracts(lacsSample)
#' predictions(model, abstracts = lacsSample)
predictions.lacs <- function(object, ..., abstracts, plus = TRUE){

  # text  <- abstracts2text(abstracts)
  # newx  <- get_dtm(text, object$vocabulary)
  newy  <- get_y(abstracts)
  truth <- ifelse(newy == 1, 1, 0) |> as.factor()


  # if(plus){
  #   predicted <- predict(object, newx = newx, s = "lambda.min", type = "class")
  #   predicted <- ifelse(predicted == 1, 1, 0) |> as.factor()
  #   class1 <- predict(object, newx = newx, s = "lambda.min", type = "response") |> as.numeric()
  #   class2 <- 1 - class1
  # } else{
  #   predicted <- predict(object$plus, newx = newx, type = "class") |> as.factor()
  #   predicted <- ifelse(predicted == 1, 1, 0) |> as.factor()
  #   class1 <- predict(object$plus, newx = newx, type = "response") |> as.numeric()
  #   class2 <- 1 - class1
  # }

  predicted <- predict(object, abstracts = abstracts, type = "class",
                       plus = plus)
  class1    <- predict(object, abstracts = abstracts, type = "response",
                       plus = plus) |> as.numeric()
  class2    <- 1 - class1

  tab <- tibble::as_tibble(
    data.frame(truth = truth,
               Class1 = class1,
               Class2 = class2,
               predicted)
  )
  return(tab)
}
