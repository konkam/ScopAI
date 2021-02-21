

#' Title
#'
#' @return
#'
ShuffleNewDeck <- function(seed=1) {
  set.seed(seed)
  sample(ordered_deck, size = length(ordered_deck), replace = F)
}

#' Title
#'
#' @param game_state
#'
#' @return
#'
#' @examples
DealBoardCards = function(game_state){
  game_state$board = game_state$deck[1:4]
  game_state$deck = game_state$deck[5:length(game_state$deck)]
  return(game_state)
}

#' Title
#'
#' @param seed
#' @param starting_player
#'
#' @return
#'
#' @examples
InitialiseGameState = function(seed = 1, starting_player = 1) {
  game_state = list(deck=ShuffleNewDeck(seed), hand1 = list(), hand2 = list(), turn = 1, stack1 = list(), stack2 = list(), last_taker = NULL) %>%
    DealPlayersCards(starting_player = starting_player) %>%
    DealBoardCards()

  return(game_state)
}
