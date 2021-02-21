

#' Title
#'
#' @return
#'
ShuffleNewDeck <- function(seed=NULL) {
  if(!is.null(seed)){
    set.seed(seed)
  }
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
InitialiseGameState = function(seed = NULL, starting_player = 1) {
  game_state = list(deck=ShuffleNewDeck(seed),
                    player1 = list(hand = c(), stack = c(), scope = 0),
                    player2 = list(hand = c(), stack = c(), scope = 0),
                    turn = 1, last_taker = NULL) %>%
    DealPlayersCards(starting_player = starting_player) %>%
    DealBoardCards()

  return(game_state)
}
