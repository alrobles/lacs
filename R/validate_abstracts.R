#' Validate a data frame with abstracts to construct an object of class abstracts via new_abstracts function
#'
#' @param abstracts_df A data frame with abstracts.
#' Should contain doi, title, abstracts and class columns in order to be valid.
#'
#' @return A validated \code{data.frame} with abstracts to construct an abstracts object
#' @export
#'
#' @examples
#' validate_abstracts(lacsSample)
validate_abstracts <- function(abstracts_df){
  stopifnot(any(names(abstracts_df) == "doi"))
  stopifnot(any(names(abstracts_df) == "title"))
  stopifnot(any(names(abstracts_df) == "abstract"))
  stopifnot(any(names(abstracts_df) == "class"))

  any(is.na(match(unique(abstracts_df$class), c("possitive", "unknow"))))

  #check that the abstract only have two classes
  classFlag <- abstracts_df$class |>
    unique() |>
    match(c("possitive", "unknown")) |>
    is.na() |>
    any()
  stopifnot(!classFlag)

  # filter only two classes
  abstracts_df <- abstracts_df[abstracts_df$class %in% c("possitive", "unknown"), ]

  return(abstracts_df[c("doi", "title", "abstract", "class")])

}
