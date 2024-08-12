#' Title
#'
#' @param x Object of the abstracts type to print.
#' @param ... Passed on to [tbl_format_setup()].#'
#' @aliases print.abstracts
#' @return An object with a `print()` method that will print the input
#'   similarly to a tibble.
#' @export
#'
#' @examples
#' print(get_abstracts(lacsSample))
#' @keywords internal
print.abstracts <- function(x, ...){
  NextMethod()
}



