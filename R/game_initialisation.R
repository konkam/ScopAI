

#' Title
#'
#' @return
#' @export
#'
#' @examples
ShuffleNewDeck <- function() {
  sample(deck, size = length(deck), replace = F)
}
ShuffleNewDeck()
