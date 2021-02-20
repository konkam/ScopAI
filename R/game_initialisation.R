

#' Title
#'
#' @return
#' @export
#'
#' @examples
ShuffleNewDeck <- function() {
  sample(ordered_deck, size = length(ordered_deck), replace = F)
}
