

#' Title
#'
#' @return
#' @export
#'
ShuffleNewDeck <- function(seed=1) {
  set.seed(seed)
  sample(ordered_deck, size = length(ordered_deck), replace = F)
}

#' Title
#'
#' @param seed
#' @param starting_player
#'
#' @return
#' @export
#'
#' @examples
InitialiseGameState = function(seed = 1, starting_player = 1) {
  game_state = list(deck=ShuffleNewDeck(seed), hand1 = list(), hand2 = list(), turn = 1, stack1 = list(), stack2 = list()) %>%
    DealPlayersCards(starting_player = starting_player) %>%
    DealBoardCards()

  return(game_state)
}
