#' Validate a data frame with abstracts to construct an object of class abstracts via new_abstracts function
#'
#' @param abstracts_df A data frame with abstracts.
#' Should contain doi, title, abstract and class columns in order to be valid.
#' The class column must contain only \code{"positive"} or \code{"unknown"} values.
#'
#' @return A validated \code{data.frame} with abstracts to construct an abstracts object
#' @export
#'
#' @examples
#' validate_abstracts(lacsSample)
validate_abstracts <- function(abstracts_df){
  required_cols <- c("doi", "title", "abstract", "class")
  missing_cols <- setdiff(required_cols, names(abstracts_df))
  if (length(missing_cols) > 0) {
    stop(paste("Missing required columns:", paste(missing_cols, collapse = ", ")))
  }

  valid_classes <- c("positive", "unknown")
  invalid_classes <- setdiff(unique(abstracts_df$class), valid_classes)
  if (length(invalid_classes) > 0) {
    stop(paste(
      "Invalid class values found:", paste(invalid_classes, collapse = ", "),
      "- allowed values are:", paste(valid_classes, collapse = ", ")
    ))
  }

  # filter to canonical classes
  abstracts_df <- abstracts_df[abstracts_df$class %in% valid_classes, ]

  return(abstracts_df[c("doi", "title", "abstract", "class")])

}
