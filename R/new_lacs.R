#' Constructor function for the class lacs.
#' A constructor for the lacs class
#'
#' @param plus An object of the class plus. Is the fit of the model
#' @param vocabulary An object of the class \code{text2vec_vocabulary}.
#' @param train An abstracts sample dataset used to train a plus model.
#' @param call The call of the function
#' @export
#' @return an object of class lacs.
#'
#' @examples
#' new_lacs(
#' plus = example_plus,
#' vocabulary = example_vocabulary,
#' train = example_train,
#' call = character()
#' )
new_lacs <- function(plus = plus::new_plus(),
                     vocabulary = data.table::data.table(),
                     train = data.frame(),
                     call = character()){

  lacs <- structure(list(plus = plus,
                         vocabulary = vocabulary,
                         train = train,
                         call = call),
                    class = "lacs")
  return(lacs)
}
