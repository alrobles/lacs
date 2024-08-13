#' Create vocabulary from abstracts
#'
#' @param abstracts An abstracts object build with get_abstracts function
#' @param term_count The minimum number of counts for an specific term in vocabulary
#'
#' @return A data.table with the vocabulary pruned by the term_count
#' @importFrom rlang .data
#' @importFrom methods is
#' @export
#'
#' @examples
#' abstracts <- get_abstracts(lacsSample)
#' v <- get_vocabulary(abstracts)
get_vocabulary <- function(abstracts, term_count = 2) {

  if(is(abstracts) == "abstracts"){
    text <- abstracts$abstract
  } else if(is.character(abstracts)){
    text <- abstracts
  } else{
    stop("Provide an abstracts object or a character vector")
  }
  text  <- utf8::utf8_encode(text)
  prep_fun <- tolower


  it = text2vec::itoken(text,
                        preprocessor = prep_fun,
                        #tok_fun = word_tokenizer,
                        progressbar = FALSE)
  stop_words <- stopwords::stopwords()

  v <-  text2vec::create_vocabulary(it,
                                    ngram = c(1L, 5L),
                                    stopwords = stop_words)
  # Filter al words starting and finishing with numbers

  v <- v |>
    dplyr::filter(!grepl(pattern = "____", .data$term )) |>
    dplyr::filter(!grepl(pattern = "^[0-9]", .data$term )) |>
    dplyr::filter(!grepl(pattern = "$[0-9]", .data$term )) |>
    # filter all terms with less than three characters
    dplyr::filter(nchar(.data$term) > 3)

  pruned_vocab = text2vec::prune_vocabulary(v, term_count_min = term_count)
  return( pruned_vocab )
}
